package edu.knowitall.tac2013.openie

import org.scalatest._
import java.io.File
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.Path;
import org.apache.solr.common.SolrInputDocument
import scala.collection.JavaConverters._
import edu.knowitall.tac2013.solr.KbpExtractionConverter

class KbpExtractionSpec extends FlatSpec {
  
  val splitDocsDir = "/samples/"
  val splitWebDocsDir = splitDocsDir + "web"
  val splitNewsDocsDir= splitDocsDir + "news"
  val splitForumDocsDir=splitDocsDir + "forum"

  val allExtrsFiles = Seq(splitNewsDocsDir, splitWebDocsDir, splitForumDocsDir)
    .map(_ + "-extrs.txt")
  
  "KbpExtractions" should "deserialize and then reserialize to original string" in {
    
    val lines = allExtrsFiles.iterator map { f => getClass.getResource(f) } map { res => io.Source.fromURL(res, "UTF8") } flatMap { _.getLines }
    
    val extrs = lines map { line => (line, KbpExtraction.read(line)) }
    
    extrs map { case (line, extr) =>
      val extrGet = extr.getOrElse(fail("Could not deserialize extraction:\n%s".format(line)))
      val reserialized = KbpExtraction.write(extrGet)
      assert(reserialized === line)
    } 
  }
  
  "KbpExtractions" should "convert to solr and back again" in {
    
    val lines = allExtrsFiles map { f => getClass.getResource(f) } map { res => io.Source.fromURL(res, "UTF8") } flatMap { _.getLines }
    
    val extrs = lines flatMap { line => KbpExtraction.read(line) }
    
    def solrDocToMap(doc: SolrInputDocument): Map[String, Any] = {
      val fields = for (field <- doc.values.asScala) yield {
        (field.getName(), field.getValue())
      }
      fields.toMap
    }
    
    
    extrs map { extr =>
      val solrDoc = KbpExtractionConverter.toSolrInputDocument(extr)
      val fieldMap = solrDocToMap(solrDoc)
      val reExtr = KbpExtractionConverter.fromFieldMap(fieldMap).getOrElse {
        val fieldMapStr = fieldMap.iterator.map { case (key, value) => s"$key -> ${value.toString}\n"}
        fail("Could not deserialize extr from fieldMap:\n%s")
      }
      // tab-serialize since we don't have a strong .equals() on KbpExtraction
      val extrSerialized = KbpExtraction.write(extr)
      val reExtrSerialized = KbpExtraction.write(reExtr)
      assert(extrSerialized === reExtrSerialized)
    } 
  }
}