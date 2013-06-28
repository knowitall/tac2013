package edu.knowitall.tac2013.openie

import org.scalatest._
import java.io.File
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.Path;


class KbpExtractionSpec extends FlatSpec {
  
  val splitDocsDir = "samples/"
  val splitWebDocsDir = splitDocsDir + "web"
  val splitNewsDocsDir= splitDocsDir + "news"
  val splitForumDocsDir=splitDocsDir + "forum"

  val allExtrsFiles = Seq(splitNewsDocsDir, splitWebDocsDir, splitForumDocsDir)
    .map(_ + "-extrs.txt")
  
  "KbpExtractions" should "deserialize and then reserialize to original string" in {
    
    val lines = allExtrsFiles map { f => getClass.getClassLoader.getResource(f) } map { res => io.Source.fromURL(res, "UTF8") } flatMap { _.getLines }
    
    val extrs = lines map { line => (line, KbpExtraction.read(line)) }
    
    extrs map { case (line, extr) =>
      val extrGet = extr.getOrElse(fail("Could not deserialize extraction:\n%s".format(line)))
      val reserialized = KbpExtraction.write(extrGet)
      assert(reserialized === line)
    } 
  }
}