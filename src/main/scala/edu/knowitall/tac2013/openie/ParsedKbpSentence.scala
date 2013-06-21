package edu.knowitall.tac2013.openie

import edu.knowitall.tool.chunk.ChunkedToken


case class ParsedKbpSentence(
    val docId: String, 
    val sentId: Int, 
    val text: String, 
    val tokens: String, 
    val postags: String, 
    val chunks: String, 
    val dgraph: String) {
  
  def chunkedTokens = {
    tokens.split(" ").zip(postags.split(" ")).zip(chunks.split(" ")).map { case  ((token, postag), chunk) =>
      new ChunkedToken(chunk, postag, token, 0)
    }
  }
  
}
    
object ParsedKbpSentence {
  
  val NUM_FIELDS = 7
  
  import KbpSentence.tabRegex
  
  def read(pickle: String): Option[ParsedKbpSentence] = read(tabRegex.split(pickle))
  
  def read(split: Array[String]): Option[ParsedKbpSentence] = {
    split match {
      case Array(docId, sentIdString, text, tokens, postags, chunks, dgraph, _*) => 
        Some(ParsedKbpSentence(docId, sentIdString.toInt, text, tokens, postags, chunks, dgraph))
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