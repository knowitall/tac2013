package edu.knowitall.tac2013.app

import edu.knowitall.tac2013.solr.query.SolrQueryType._
import edu.knowitall.tac2013.openie.KbpExtraction
import edu.knowitall.tac2013.openie.KbpArgument
import edu.knowitall.tac2013.openie.KbpExtractionField
import edu.knowitall.tac2013.openie.WikiLink
import edu.knowitall.tac2013.solr.query.SolrQuery
import edu.knowitall.tool.chunk.ChunkedToken
import edu.knowitall.taggers.Type
import edu.knowitall.collection.immutable.Interval
import edu.knowitall.tac2013.app.LocationHelper.findLocationTaggedType

class TrimmedType(var string: String, var interval: Interval){
  
  def setString(newString: String){string = newString}
  def setInterval(newInterval: Interval){interval = newInterval}
}

class Candidate(val id: Int, val solrQuery: SolrQuery, val extr: KbpExtraction, val types: List[Type]) {
  
  import Candidate._

  def pattern = solrQuery.pattern 
  def queryType = solrQuery.queryType

  def debugString = {
    
    val trimLinkString = fillField.wikiLink match {
      case Some(wikiLink) => " [" + wikiLink.nodeId.getOrElse(wikiLink.fbid) + "]"
      case None => ""
    }
    
    val trimString = trimmedFill.string + trimLinkString
    
    "fill: " + trimString + "\tentity: " + trimmedEntity.string +
    "\trel: " + extr.rel.debugString + "\targ1: " + extr.arg1.debugString + 
    "\targ2: " + extr.arg2.debugString + "\t docID: " + extr.sentence.docId +
      "\tconf: " + extr.confidence + "\t sent: " + extr.sentence.dgraph.text
  }
  
  def deduplicationKey: String = Seq(extractionKey, extr.sentence.dgraph.text).mkString(" ")
  
  /**
   * Concatenates tokens from (arg1, rel, arg2) which are nouns, pronouns, or verbs
   * If arg1 or arg2 is linked, uses fbid for that field instead.
   */
  def extractionKey: String = {

    def tokenKey(tokens: TraversableOnce[ChunkedToken]): String = {
      tokens.filter(tok => tok.isNoun || tok.isPronoun || tok.isVerb).map(_.string).mkString(" ")
    }

    def argKey(arg: KbpArgument) = arg.wikiLink match {
      case Some(wikiLink) => wikiLink.fbid
      case None => tokenKey(arg.tokens)
    }

    val arg1Key = extr.arg1.originalText // argKey(extr.arg1)
    val relKey = extr.rel.originalText
    val arg2Key = extr.arg2.originalText // argKey(extr.arg2)

    Seq(arg1Key, relKey, arg2Key).mkString(", ")
  }
  
  val entityField = pattern.entityIn match {
    case Some("arg1") => extr.arg1
    case Some("arg2") => extr.arg2
    case Some("relation") => extr.rel
    case _ => throw new RuntimeException("Invalid entityIn for pattern: %s".format(pattern.debugString))
  }
  
  val fillField = pattern.slotFillIn match {
    case Some("arg1") => extr.arg1
    case Some("arg2") => extr.arg2
    case Some("relation") => extr.rel
    case _ => throw new RuntimeException("Invalid slotFillIn for pattern: %s".format(pattern.debugString))
  }
  
  def offsetString(interval: Interval): String = "%d-%d".format(interval.start, interval.last)
  
  def offsetString(trimmedItem: TrimmedType): String = offsetString(getOffset(trimmedItem))
  
  def offsetString(field: KbpExtractionField): String = offsetString(getOffset(field))
  
  def getOffset(fill: TrimmedType): Interval = {
    val startOffset = extr.sentence.startOffset
    val firstToken = extr.sentence.chunkedTokens(fill.interval).minBy(_.offset)
    val lastToken = extr.sentence.chunkedTokens(fill.interval).maxBy(t => t.offset + t.string.length)
    Interval.closed(firstToken.offset + startOffset, lastToken.offset + lastToken.string.length + startOffset - 1)
  }
  
  def getOffset(field: KbpExtractionField): Interval = {
    val startOffset = extr.sentence.startOffset
    val firstToken = field.tokens.minBy(_.offset)
    val lastToken = field.tokens.maxBy(t => t.offset + t.string.length)
    Interval.closed(firstToken.offset + startOffset, lastToken.offset + lastToken.string.length + startOffset - 1)
  }
  
  private def basicTrim(str: String, interval: Interval): TrimmedType = {    
    val noChangeTrimmedFill = new TrimmedType(str,interval)
    
    //if there is only one word just return the basic
    var words = str.split(" ")
    if(words.length ==1){
      return noChangeTrimmedFill
    }
    
    //get chunked tokens matching the input interval, if for some
    //reason something is wrong just return the default trimmedFill
    val tokens =  extr.sentence.chunkedTokens(interval)
    if(tokens.isEmpty){
      return noChangeTrimmedFill
    }
    
    //get the first token and check if it is a preposition, if it is a
    //preposition remove it from the TrimmedFill and define a new interval
    //for the new TrimmedFill
    val headToken = tokens.head
    if(headToken.isPreposition){
      val noPrepSlotFillString = str.substring(headToken.offsets.size).trim()
      val newInterval = Interval.open(interval.start+1,interval.end)
      return new TrimmedType(noPrepSlotFillString,newInterval)
    }
    
    return noChangeTrimmedFill
    
     
  }
  
  // intersectingTypes can't be empty when called
  private def chooseBestInterval (intersectingTypes: List[Type]): Option[Type] ={
    val slotType = pattern.slotType.getOrElse("")
    if(slotType == "Country" || slotType == "Stateorprovince" ||
      slotType == "City"){
      findLocationTaggedType(intersectingTypes,slotType)
    }
    else{
      // return first type in list
      Some(intersectingTypes.head)
    }
  }
  
  private def getTrimmedFill(): TrimmedType = {

    val intersectingTypes = types.filter(_.interval.intersects(fillField.tokenInterval))
    if (intersectingTypes.isEmpty) {
      basicTrim(fillField.originalText, fillField.tokenInterval)
    } //there are types from the tagger that was ran on
    //the appropriate slot fill type
    else {
      chooseBestInterval(intersectingTypes) match {
        case Some(bestType) => {
          // Type doesn't seem to correctly return text()
          val bestTokens = extr.sentence.chunkedTokens(bestType.interval)
          val text = originalText(bestTokens)
          basicTrim(text, bestType.interval)
        }
        case None => {
          basicTrim(fillField.originalText, fillField.tokenInterval)
        }
      }
    }
  }
  
  private def chooseBestEntityInterval (intersectingTypes: List[Type]): Option[Type] ={
    val entityType = solrQuery.pattern.slotName.split(":")(0)
    entityType match {
      case "org" => { 
        for(t <- intersectingTypes){
          if(t.descriptor() == "StanfordORGANIZATION"){
            return Some(t)
          }
        }
        return None
      }
      case "per" => {
        for(t <- intersectingTypes){
          if(t.descriptor() == "StanfordPERSON"){
            return Some(t)
          }
        }
        return None
      }
    }
  }
  
  private def getTrimmedEntity(): TrimmedType = {

    //types come in right to left order so the first element is the right most
    val entityTypes = SemanticTaggers.useStandfordNERTagger(extr.sentence.chunkedTokens)
    val intersectingTypes = entityTypes.filter(_.interval.intersects(entityField.tokenInterval))
    if (intersectingTypes.isEmpty) {
      basicTrim(entityField.originalText, entityField.tokenInterval)
    } 
    else {
      chooseBestEntityInterval(intersectingTypes) match {
        case Some(bestType) => {
          // Type doesn't seem to correctly return text()
          val bestTokens = extr.sentence.chunkedTokens(bestType.interval)
          val text = originalText(bestTokens)
          basicTrim(text, bestType.interval)
        }
        case None => {
          basicTrim(entityField.originalText, entityField.tokenInterval)
        }
      }
    }
  }
  
  
  
  lazy val entityOffsetInterval = getOffset(trimmedEntity)
  
  lazy val fillOffsetInterval = getOffset(trimmedFill)
  
  lazy val relOffsetInterval = getOffset(extr.rel)
  
  lazy val justificationInterval =  {
     Interval.span(Iterable(entityOffsetInterval, fillOffsetInterval, relOffsetInterval)) 
  }
  
  lazy val entityOffsetString = offsetString(trimmedEntity)
  
  lazy val fillOffsetString = offsetString(trimmedFill)
  
  lazy val relOffsetString = offsetString(extr.rel)
  
  lazy val justificationOffsetString = offsetString(justificationInterval)
  
  lazy val trimmedFill = getTrimmedFill()
  
  lazy val trimmedEntity = getTrimmedEntity()

}

object Candidate {
  
  import edu.knowitall.tool.tokenize.Token
  
  type Candidates = Seq[Candidate]
  
  def groupScore(candidates: Candidates): Double = {
    
    require(candidates.nonEmpty, "Invalid argument, empty candidates.")
    
    1 - candidates.map(c => 1 - c.extr.confidence).reduce(_ * _)
  }

  def originalText(tokens: Iterable[Token]) = {
    val builder = new StringBuilder()
    
    for (token <- tokens) {
      builder.append(" " * (token.offset - builder.length - tokens.head.offset))
      builder.append(token.string)
    }

    builder.toString()
  }
}