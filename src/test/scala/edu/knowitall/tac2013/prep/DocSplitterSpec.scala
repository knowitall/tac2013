package edu.knowitall.tac2013.prep

import org.scalatest._
import java.io.File
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.Path;


class DocSplitterSpec extends FlatSpec {

  val splitDocsDir = "src/main/resources/samples/docs-split/"
  val splitWebDocsDir = splitDocsDir + "web"
  val splitNewsDocsDir= splitDocsDir + "news"
  val splitForumDocsDir=splitDocsDir + "forum"
  
  val allDocsDirs = Seq(splitWebDocsDir, splitNewsDocsDir, splitForumDocsDir)
  
  "DocSplitter" should "Tag lines with correct byte offsets" in {
    
    val files = allDocsDirs.flatMap(dir => new File(dir).listFiles())
    for (file <- files) {
      
      testFile(file)
    }
  }
  
  /*
   * Assumes that a file contains a single kbp doc
   */
  def testFile(file: File): Unit = {
    
    val source = io.Source.fromFile(file, "UTF8")
    
    val spliterator = DocSplitter(source.getLines)
    require(spliterator.hasNext)
    
    val kbpDoc = spliterator.next()
    
    require(!spliterator.hasNext)
    
    val fileString = DocSplitterSpec.fileString(file)
    
    assert(fileString.equals(kbpDoc.getString))
    
    for (kbpline <- kbpDoc.lines) {
      val targetString = fileString.drop(kbpline.offset).take(kbpline.length)
      assert(targetString.equals(kbpline.line), "Not equal:\n%s\n%s".format(targetString, kbpline.line))
    }
    source.close()
  }
}

object DocSplitterSpec {
   
  def fileString(file: File): String = {
    val path = Paths.get(file.getPath)
    new String(Files.readAllBytes(path), "UTF8");
  }
}