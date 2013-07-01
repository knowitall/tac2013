package edu.knowitall.tac2013.prep

import org.scalatest._
import util.LineReader

object KbpSentenceSpec {
  val samplesDir = "/samples/"
  val corpora = Seq("news", "web", "forum")
  val sentFiles = corpora.map { c => "%s%s%s".format(samplesDir, c, "-sentences.txt") }
  val rawFiles = corpora.map { c => "%s%s%s".format(samplesDir, c, "-xml.txt") }
}

class KbpSentenceSpec extends FlatSpec {

  import KbpSentenceSpec._
  
  "KbpSentences" should "deserialize then serialize to their original string" in {
    
    val testSrcs = sentFiles map { f => getClass.getResource(f) } map { res => scala.io.Source.fromURL(res, "UTF8") }
    val lines = testSrcs.flatMap(_.getLines)
    lines.foreach { line =>
      val sent = KbpSentence.read(line).get
      val reserialized = KbpSentence.write(sent)
      assert(reserialized === line)
    } 
  }
  
  "KbpSentences" should "have offsets that correctly key into source doc" in {
    
    val sentenceMap = {
      val testSrcs = sentFiles map { f => getClass.getResource(f) } map { res => scala.io.Source.fromURL(res, "UTF8") }
      val lines = testSrcs.flatMap(_.getLines)
      val sents = lines.flatMap { s => KbpSentence.read(s) } filter { !_.text.startsWith("This post was written") }
      sents groupBy { s => s.docId }
    }

    corpora.zip(rawFiles) foreach {
      case (corpus, sample) => 
        val lineReader = LineReader.fromURL(getClass.getResource(sample), "UTF8")
        val docProcessor = KbpDocProcessor.getProcessor(corpus)
        val docSplitterator = new DocSplitter(lineReader)
        for (rawDoc <- docSplitterator; parsedDoc <- docProcessor.process(rawDoc); docId <- parsedDoc.extractDocId) {
          val rawString = rawDoc.getString
          val sents = sentenceMap(docId)
          sents foreach { s =>
            val extrString = util.Asciifier(rawString.drop(s.offset).take(s.text.length).replaceAll("\n", " "))
            if (!extrString.equals(s.text)) {
              System.err.println("\"%s\"".format(extrString))
              System.err.println("\"%s\"".format(s.text))
              fail()
            }
          }
        }
        lineReader.close()
    }
  }
}