package edu.knowitall.tac2013.prep

import org.scalatest._
import java.io.File
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.Path;
import java.net.URL


class DocSplitterSpec extends FlatSpec {

  val splitDocsDir = "samples/docs-split/"
  val splitWebDocsDir = splitDocsDir + "web"
  val splitNewsDocsDir= splitDocsDir + "news"
  val splitForumDocsDir=splitDocsDir + "forum"
  
  val allDocsDirs = Seq(splitWebDocsDir, splitNewsDocsDir, splitForumDocsDir)
  
  "DocSplitter" should "Tag lines with correct byte offsets" in {
    
    val resourceDirs = allDocsDirs.map(res => getClass().getClassLoader().getResource(res))
    
    val resources = resourceDirs.flatMap(url => new File(url.getFile()).listFiles).map(_.toURL)
    
    for (res <- resources) {
      
      testFile(res)
    }
  }
  
  /*
   * Assumes that a file contains a single kbp doc
   */
  def testFile(res: URL): Unit = {
    
    val source = io.Source.fromURL(res)
    
    val spliterator = DocSplitter(source.getLines)
    require(spliterator.hasNext)
    
    val kbpDoc = spliterator.next()
    
    require(!spliterator.hasNext)
    
    val fileString = DocSplitterSpec.fileString(res)
    
    assert(fileString.equals(kbpDoc.getString))
    
    for (kbpline <- kbpDoc.lines) {
      val targetString = fileString.drop(kbpline.offset).take(kbpline.length)
      assert(targetString.equals(kbpline.line), "Not equal:\n%s\n%s".format(targetString, kbpline.line))
    }
  }
}

object DocSplitterSpec {
   
  def fileString(file: URL): String = {
    val path = Paths.get(file.getPath)
    new String(Files.readAllBytes(path), "UTF8");
  }
}