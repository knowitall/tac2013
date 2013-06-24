package edu.knowitall.tac2013.prep

import edu.knowitall.tool.chunk.ChunkedToken
import edu.knowitall.tool.chunk.Chunker
import edu.knowitall.tool.tokenize.Tokenizer
import KbpSentence.tabRegex
import scala.Array.canBuildFrom


case class ParsedKbpSentence(
    val docId: String,
    val sentNum: String,
    val startOffset: String,
    val tokens: String, 
    val postags: String, 
    val chunks: String, 
    val tokenOffsets: String,
    val dgraph: String) {
  
  import ParsedKbpSentence.wsSplit
  
  lazy val startOffsetInt = startOffset.toInt
  
  def chunkedTokens = {
    
    val ts = wsSplit.split(tokens)
    val ps = wsSplit.split(postags)
    val cs = wsSplit.split(chunks)
    val os = wsSplit.split(tokenOffsets)
    
    ts.zip(ps).zip(cs).zip(os) map { case (((token, postag), chunk), offset) =>
      new ChunkedToken(chunk, postag, token, offset.toInt)
    }
  }
  
}
    
object ParsedKbpSentence {
  
  private val wsSplit = "\\s+".r
  
  val NUM_FIELDS = 8
  
  import KbpSentence.tabRegex
  
  def read(pickle: String): Option[ParsedKbpSentence] = read(tabRegex.split(pickle))
  
  def read(split: Array[String]): Option[ParsedKbpSentence] = {
    split match {
      case Array(docId, sentNum, startOffset, tokens, postags, chunks, tokenOffsets, dgraph, _*) => 
        Some(ParsedKbpSentence(docId, sentNum, startOffset, tokens, postags, chunks, tokenOffsets, dgraph))
      case _ => {
        System.err.println("Error reading ParsedKbpSentence: %s".format(split.mkString("\t")))
        None
      }
    }
  }
  
  def write(parsedKbpSentence: ParsedKbpSentence): String = {
    ParsedKbpSentence.unapply(parsedKbpSentence).get.productIterator.map(_.toString).mkString("\t")
  }
}