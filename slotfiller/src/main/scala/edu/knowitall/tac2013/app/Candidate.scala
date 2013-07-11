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
import edu.knowitall.tac2013.findSlotFillersApp.LocationHelper.findLocationTaggedType

class TrimmedFill(val string: String, val interval: Interval)

class Candidate(val id: Int, val solrQuery: SolrQuery, val extr: KbpExtraction, val types: List[Type]) {

  def pattern = solrQuery.pattern 
  def queryType = solrQuery.queryType

  def debugString = {
    
    val trimLinkString = fillField.wikiLink match {
      case Some(wikiLink) => " [" + wikiLink.nodeId.getOrElse(wikiLink.fbid) + "]"
      case None => ""
    }
    
    val trimString = trimmedFill.string + trimLinkString
    
    "fill: " + trimString + "\tentity: " + entityField.debugString +
    "\trel: " + extr.rel.debugString + "\t docID: " + extr.sentence.docId +
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
  
  def offsetString(fill: TrimmedFill): String = offsetString(getOffset(trimmedFill))
  
  def offsetString(field: KbpExtractionField): String = offsetString(getOffset(field))
  
  def getOffset(fill: TrimmedFill): Interval = {
    val startOffset = extr.sentence.startOffset
    val firstToken = extr.sentence.chunkedTokens(fill.interval).minBy(_.offset)
    val lastToken = extr.sentence.chunkedTokens(fill.interval).maxBy(t => t.offset + t.string.length)
    Interval.closed(firstToken.offset + startOffset, lastToken.offset + lastToken.string.length + startOffset)
  }
  
  def getOffset(field: KbpExtractionField): Interval = {
    val startOffset = extr.sentence.startOffset
    val firstToken = field.tokens.minBy(_.offset)
    val lastToken = field.tokens.maxBy(t => t.offset + t.string.length)
    Interval.closed(firstToken.offset + startOffset, lastToken.offset + lastToken.string.length + startOffset)
  }
  
  private def basicTrim(str: String, interval: Interval): TrimmedFill = {    
    val noChangeTrimmedFill = new TrimmedFill(str,interval)
    
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
      return new TrimmedFill(noPrepSlotFillString,newInterval)
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
  
  private def getTrimmedFill(): TrimmedFill = {
    var trimmedFillString: Option[String] = None
    
    if(types.isEmpty){
      return basicTrim(fillField.originalText,fillField.tokenInterval)
    }
    
    //there are types from the tagger that was ran on
    //the appropriate slot fill type
    else{
      var intersectingTypes = List[Type]()
      for(t <- types){
        if(t.interval().intersects(fillField.tokenInterval)){
          intersectingTypes = intersectingTypes ::: List(t)
        }
      }
      if(!intersectingTypes.isEmpty){
        val bestInterval = chooseBestInterval(intersectingTypes)
        if(bestInterval.isDefined){
           return basicTrim(bestInterval.get.text(),bestInterval.get.interval())
        }
        else{
          return basicTrim(fillField.originalText,fillField.tokenInterval)
        }
      }
      else{
        return basicTrim(fillField.originalText,fillField.tokenInterval)
      }
    }
  }
  
  
  
  lazy val entityOffsetInterval = getOffset(entityField)
  
  lazy val fillOffsetInterval = getOffset(fillField)
  
  lazy val relOffsetInterval = getOffset(extr.rel)
  
  lazy val justificationInterval =  {
     Interval.span(Iterable(entityOffsetInterval, fillOffsetInterval, relOffsetInterval)) 
  }
  
  lazy val entityOffsetString = offsetString(entityField)
  
  lazy val fillOffsetString = offsetString(trimmedFill)
  
  lazy val relOffsetString = offsetString(extr.rel)
  
  lazy val justificationOffsetString = offsetString(justificationInterval)
  
  lazy val trimmedFill = getTrimmedFill()
}

object Candidate {
  
  type Candidates = Seq[Candidate]
  
  def groupScore(candidates: Candidates): Double = {
    
    require(candidates.nonEmpty, "Invalid argument, empty candidates.")
    
    1 - candidates.map(c => 1 - c.extr.confidence).reduce(_ * _)
  }
  
}