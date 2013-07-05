package edu.knowitall.tac2013.findSlotFillersApp

import QueryType._
import edu.knowitall.tac2013.openie.KbpExtraction
import edu.knowitall.tac2013.openie.KbpArgument
import edu.knowitall.tac2013.openie.KbpExtractionField
import edu.knowitall.tac2013.openie.WikiLink
import edu.knowitall.tool.chunk.ChunkedToken
import edu.knowitall.taggers.Type
import edu.knowitall.collection.immutable.Interval

class Candidate(val pattern: SlotPattern, val queryType: QueryType, val extr: KbpExtraction, val types: List[Type]) {

  /**
   * Concatenates tokens from (arg1, rel, arg2) which are nouns, pronouns, or verbs
   * If arg1 or arg2 is linked, uses fbid for that field instead.
   */
  def extractionKey: String = {

    def tokenKey(tokens: TraversableOnce[ChunkedToken]): String = {
      tokens.filter(tok => tok.isNoun || tok.isPronoun || tok.isVerb).map(_.string).mkString(" ")
    }

    def argKey(arg: KbpArgument) = arg.wikiLink match {
      case Some(WikiLink(name, fbid, nodeIdOpt)) => fbid
      case None => tokenKey(arg.tokens)
    }

    val arg1Key = argKey(extr.arg1)
    val relKey = tokenKey(extr.rel.tokens)
    val arg2Key = argKey(extr.arg2)

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
  
  def offsetString(field: KbpExtractionField): String = {
    val startOffset = extr.sentence.startOffset
    val firstToken = field.tokens.minBy(_.offset)
    val lastToken = field.tokens.maxBy(t => t.offset + t.string.length - 1)
    "%d-%d".format(firstToken.offset + startOffset, lastToken.offset + lastToken.string.length + startOffset - 1)
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
  private def getTrimmedFill(): TrimmedFill = {
    var trimmedFillString: Option[String] = None
    
    if(types.isEmpty){
      return basicTrim(fillField.originalText,fillField.tokenInterval)
    }
    
    //there are types from the tagger that was ran on
    //the appropriate slot fill type
    else{
      for(t <- types){
        if(t.interval().intersects(fillField.tokenInterval)){
          return basicTrim(t.text(),t.interval())
        }
      }
      return basicTrim(fillField.originalText,fillField.tokenInterval)
    }
  }
  
  class TrimmedFill(val trimmedFillString: String, val trimmedFillInterval: Interval)
  
  
  val entityOffsetString = offsetString(entityField)
  
  val fillOffsetString = offsetString(fillField)
  
  val relOffsetString = offsetString(extr.rel)
  
  val trimmedFill = getTrimmedFill()
}