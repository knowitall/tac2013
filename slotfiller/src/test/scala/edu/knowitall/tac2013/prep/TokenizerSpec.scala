package edu.knowitall.tac2013.prep

import org.scalatest._
import edu.knowitall.tool.tokenize.Tokenizer
import edu.knowitall.tool.tokenize.OpenNlpTokenizer
import edu.knowitall.tool.chunk.ChunkedToken
import edu.knowitall.tool.chunk.OpenNlpChunker
import edu.knowitall.tool.postag.OpenNlpPostagger
import java.util.regex.Pattern

class TokenizerSpec extends FlatSpec {

  import KbpSentenceSpec._
  
  import util.Asciifier.whitespace_charclass 
  val wsPattern = Pattern.compile(whitespace_charclass)
  def fixWs(str: String): String = wsPattern.matcher(str).replaceAll(" ")
  
  val chunker = {
    val chunkerModel = OpenNlpChunker.loadDefaultModel
    val postagModel = OpenNlpPostagger.loadDefaultModel
    val tokenModel = OpenNlpTokenizer.loadDefaultModel
    val tokenizer = new OpenNlpTokenizer(tokenModel)
    val postagger = new OpenNlpPostagger(postagModel, tokenizer)
    new OpenNlpChunker(chunkerModel, postagger)
  }
  
  "A tokenizer" should "annotate tokens with correct character offsets" in {
    
    val testSrcs = sentFiles map { f => getClass.getResource(f) } map { res => scala.io.Source.fromURL(res, "UTF8") }
    val lines = testSrcs.flatMap(_.getLines.take(300))
    lines.foreach { line =>
      val sent = KbpSentence.read(line).get
      val chunked = chunker(sent.text)
      test(chunked, sent)
    } 
  }

  def test(chunked: Seq[ChunkedToken], source: KbpSentence): Unit = {
    val originalText = fixWs(Tokenizer.originalText(chunked))
    val srcTrim = fixWs(source.text.trim)
    val zip = originalText.zip(srcTrim).zipWithIndex
    zip.find({ case ((c1, c2), i) => c1 != c2 }) match {
      case Some(((c1, c2), i)) => {
        System.err.println("%s:%s Sentence mismatch at index %d:".format(source.docId, source.offset, i))
        System.err.println("sent: \"%s\"".format(srcTrim.getBytes("UTF8").mkString(" ")))
        System.err.println("toks: \"%s\"".format(originalText.getBytes("UTF8").mkString(" ")))
        fail()
      }
      case None => {}
    }
    
    for (token <- chunked) {
      val substr = source.text.drop(token.offset).take(token.string.length)
      if (!substr.equals(token.string)) {
        System.err.println("%s:%s".format(source.docId, source.offset.toInt + token.offset))
        System.err.println("file:  \"%s\"".format(substr))
        System.err.println("token: \"%s\"".format(token.string))
        fail()
      }
    }
  }
}