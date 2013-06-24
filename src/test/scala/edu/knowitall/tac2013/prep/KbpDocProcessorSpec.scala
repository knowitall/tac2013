package edu.knowitall.tac2013.prep

import org.scalatest._
import java.io.File
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.Path;


class KbpDocProcessorSpec extends FlatSpec {
  
  val docSplitter = new DocSplitter()
  val splitDocsDir = "src/main/resources/samples/docs-split/"
  val splitWebDocs = (splitDocsDir, "web")
  val splitNewsDocs= (splitDocsDir, "news")
  val splitForumDocs=(splitDocsDir, "forum")
  
  val allDocsDirs = Seq(splitWebDocs, splitNewsDocs, splitForumDocs)
  
  "DocSplitter" should "Tag lines with correct byte offsets" in {
    
    val files = allDocsDirs.flatMap { case (dir, corpus) => 
      new File(dir+corpus).listFiles().map(f => (f, KbpDocProcessor.getProcessor(corpus)))
    }
      
    for ((file, parser) <- files) {
      
      testFile(file, parser)
    }
  }
  
  /*
   * Assumes that a file contains a single kbp doc
   */
  def testFile(file: File, docParser: KbpDocProcessor): Unit = {
    
    val source = io.Source.fromFile(file)
    
    val spliterator = docSplitter.splitDocs(source)
    require(spliterator.hasNext)
    
    val kbpDoc = spliterator.next()
    
    require(!spliterator.hasNext)
    
    val parsedDoc = docParser.process(kbpDoc)
    
    val fileString = DocSplitterSpec.fileString(file)
    
    for (doc <- parsedDoc.toList; kbpline <- doc.textLines) {
      val targetString = fileString.drop(kbpline.offset).take(kbpline.length)
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