package edu.knowitall.tac2013.prep

import org.scalatest._
import java.io.File
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.Path;
import util.LineReader

class DocSplitterSpec extends FlatSpec {

  val splitDocsDir = "/samples/docs-split/"
  val splitWebDocsDir = splitDocsDir + "web"
  val splitNewsDocsDir= splitDocsDir + "news"
  val splitForumDocsDir=splitDocsDir + "forum"
  
  val allDocsDirs = Seq(splitWebDocsDir, splitNewsDocsDir, splitForumDocsDir)
  
  "DocSplitter" should "Tag lines with correct byte offsets" in {
    
    val urls = allDocsDirs.flatMap { dir => 
      new File(getClass.getResource(dir).getFile()).listFiles.map(_.toURL)
    }
    for (url <- urls) {
      
      testFile(url)
    }
  }
  
  /*
   * Assumes that a file contains a single kbp doc
   */
  def testFile(url: java.net.URL): Unit = {
    
    val lineReader = LineReader.fromURL(url, "UTF8")
    val spliterator = new DocSplitter(lineReader)
    require(spliterator.hasNext)
    
    val kbpDoc = spliterator.next()
    
    require(!spliterator.hasNext)
    
    val fileString = DocSplitterSpec.fileString(url)
    
    assert(fileString.equals(kbpDoc.getString))
    
    for (kbpline <- kbpDoc.lines) {
      val targetString = fileString.drop(kbpline.offset).take(kbpline.length)
      assert(targetString.equals(kbpline.line), "Not equal:\n%s\n%s".format(targetString, kbpline.line))
    }
    lineReader.close()
  }
}

object DocSplitterSpec {
   
  def fileString(url: java.net.URL): String = {
    val path = Paths.get(url.getFile)
    new String(Files.readAllBytes(path), "UTF8");
  }
}