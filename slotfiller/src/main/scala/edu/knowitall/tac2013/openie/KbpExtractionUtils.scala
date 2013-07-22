package edu.knowitall.tac2013.openie

import edu.knowitall.collection.immutable.Interval
import edu.knowitall.tac2013.prep.ParsedKbpSentence

object KbpExtractionUtils {

  
  def getOffset(field: KbpExtractionField, sentence :ParsedKbpSentence): Interval = {
    val startOffset = sentence.startOffset
    val firstToken = field.tokens.minBy(_.offset)
    val lastToken = field.tokens.maxBy(t => t.offset + t.string.length)
    Interval.closed(firstToken.offset + startOffset, lastToken.offset + lastToken.string.length + startOffset - 1)
  }
}