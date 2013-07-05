package edu.knowitall.tac2013.findSlotFillersApp

import QueryType._
import edu.knowitall.tac2013.openie.KbpExtraction
import edu.knowitall.tac2013.openie.KbpArgument
import edu.knowitall.tac2013.openie.KbpExtractionField
import edu.knowitall.tac2013.openie.WikiLink
import edu.knowitall.tool.chunk.ChunkedToken

class Candidate(val pattern: SlotPattern, val queryType: QueryType, val extr: KbpExtraction) {

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
  
  val entityOffsetString = offsetString(entityField)
  
  val fillOffsetString = offsetString(fillField)
  
  val relOffsetString = offsetString(extr.rel)
}