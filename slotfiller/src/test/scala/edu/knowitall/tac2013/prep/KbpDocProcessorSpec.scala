package edu.knowitall.tac2013.prep

import org.scalatest._
import java.io.File
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.Path;


class KbpDocProcessorSpec extends FlatSpec {
  
  val splitDocsDir = "/samples/docs-split/"
  val splitWebDocs = (splitDocsDir, "web")
  val splitNewsDocs= (splitDocsDir, "news")
  val splitForumDocs=(splitDocsDir, "forum")
  
  val allDocsDirs = Seq(splitWebDocs, splitNewsDocs, splitForumDocs)
  
  "KbpDocProcessor" should "Tag lines with correct byte offsets" in {
    
    val urls = allDocsDirs.flatMap { case (dir, corpus) => 
      val resDir = new File(getClass.getResource(dir + corpus).getFile()).listFiles.map(_.toURL)
      resDir map ( res => (res, KbpDocProcessor.getProcessor(corpus)) )
    }
    for ((url, parser) <- urls) {
      
      testFile(url, parser)
    }
  }
  
  /*
   * Assumes that a file contains a single kbp doc
   */
  def testFile(url: java.net.URL, docParser: KbpDocProcessor): Unit = {
    
    val source = io.Source.fromURL(url)
    
    val spliterator = DocSplitter(source.getLines)
    require(spliterator.hasNext)
    
    val kbpDoc = spliterator.next()
    
    require(!spliterator.hasNext)
    
    val parsedDoc = docParser.process(kbpDoc)
    
    val fileString = DocSplitterSpec.fileString(url)
    
    for (doc <- parsedDoc.toList; kbpline <- doc.textLines) {
      val targetString = fileString.drop(kbpline.offset).take(kbpline.length)
      val docId = doc.extractDocId.getOrElse(fail("Couldn't extract docId: %s".format(doc.docIdLine)))
      if (docId.startsWith(" ") || docId.endsWith(" ")) fail("docId: \"%s\" should not start or end with whitespace.".format(docId))
      if (!kbpline.line.trim().isEmpty) {
        if (!targetString.equals(kbpline.line)) {
          System.err.println("ParsedDoc: error on docId=%s expected\\nactual\n%s\n%s".format(doc.docIdLine.line, targetString, kbpline.line))
          fail()
        }
      }
    }
    source.close()
  }
}