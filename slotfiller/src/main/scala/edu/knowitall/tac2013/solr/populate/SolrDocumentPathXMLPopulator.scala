package edu.knowitall.tac2013.solr.populate

import org.apache.solr.client.solrj.impl.ConcurrentUpdateSolrServer
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream
import edu.knowitall.tac2013.openie._
import edu.knowitall.tac2013.prep._
import java.util.concurrent.atomic.AtomicInteger
import edu.knowitall.tac2013.prep.util.FileUtils
import scopt.OptionParser
import edu.knowitall.tac2013.prep.util.Line
import edu.knowitall.tac2013.prep.util.LineReader
import edu.knowitall.tac2013.solr.KbpExtractionConverter
import scala.Option.option2Iterable
import org.apache.solr.common.SolrInputDocument
import scala.io.Source
import java.io.FileInputStream

class SolrDocumentPathXMLPopulator private (val solrServer: ConcurrentUpdateSolrServer) {

  private val errorCounter = new AtomicInteger(0)
  private val extrCounter = new AtomicInteger(0)

  private def tryAddSingle(docId: String, xml: String): Unit = {
    try {
      addSingle(docId,xml)
    } catch {
      case e: Exception => {
        System.err.println("Error #%d: %s".format(errorCounter.incrementAndGet(), e.getMessage()))
        e.printStackTrace()
      }
    }
  }

  private def addSingle(docId: String, xml: String): Unit = {
    val doc = new SolrInputDocument()
    doc.addField("docid", docId)
    doc.addField("xml", xml)
    
    solrServer.add(doc)
    if (extrCounter.incrementAndGet() % 10000 == 0) {
      solrServer.commit()
      System.err.println("SolrPopulator: %d documents added.".format(extrCounter.get))
    }
  }
  
  private def populate(docs: Iterator[(String,String)]): Unit = {
    
    //delete all old documents
    solrServer.deleteByQuery("*:*")
    solrServer.commit()
    
    //add all new documents
    docs.foreach(f => tryAddSingle(f._1,f._2))
    solrServer.commit()
    solrServer.shutdown()
  }
}

object SolrDocumentPathXMLPopulator {
  
  private val queueSize = 1000
  private val threadCount = 4
  
  private def getDefaultSolrServer(solrUrl: String): ConcurrentUpdateSolrServer = {
    new ConcurrentUpdateSolrServer(solrUrl, queueSize, threadCount)
  }
  
  def populate(extrs: Iterator[(String,String)], solrUrl: String): Unit = {
    val server = getDefaultSolrServer(solrUrl)
    val populator = new SolrDocumentPathXMLPopulator(server)
    populator.populate(extrs)
  }
  
  def main(args: Array[String]): Unit = {
    
    var inputPath = ""
    var solrUrl = "."
    
    val parser = new OptionParser("SolrDocumentPathXMLPopulator") {
      arg("inputPath", "recursive dir.", { s => inputPath = s})
      arg("solrUrl", "URL to Solr instance.", { s => solrUrl = s})
    }

    if (!parser.parse(args)) return
    
    val input =  {
      if (inputPath.equals("")) 
        return
      else {
        val files = FileUtils.getFilesRecursive(new java.io.File(inputPath))
        val pathXMLPairs = files.map(f => {getIdXmlTuple(f)})
        pathXMLPairs.flatten
      }
    }

    populate(input, solrUrl)
    
  }
  
  /**
   * returns an xml string for each doc id
   */
  def getIdXmlTuple(f : java.io.File): Iterator[(String,String)] = {
    // docSplitter changes depending on if the files are zipped or not
    var docSplitter : Option[DocSplitter] = None
    if(f.getName().contains(".gz")){
      docSplitter = Some( new DocSplitter(LineReader.fromInputStream(new GzipCompressorInputStream(new FileInputStream(f)), "UTF8")))
    }
    else{
      docSplitter = Some(new DocSplitter(LineReader.fromFile(f,"UTF8")))
    }
    val idXmlPairs = for (d <- docSplitter.get) yield {
        val lines = d.lines
        var pair :Option[(String,String)] = None
        val docIdLineOption = lines.filter(l => l.line.startsWith("<DOCID>")).headOption
        if(docIdLineOption.isDefined){
          val processedKbpDoc = new KbpProcessedDoc(docIdLineOption.get,None,None,lines)
          val docIdOption = processedKbpDoc.extractDocId
          val xmlString = d.getString
          if(docIdOption.isDefined){
            pair = Some((docIdOption.get,xmlString))
          }
          else{
            pair = None
          }
          
        }
        else{
          pair = None
        }
        pair
    }
    idXmlPairs.flatten
  }
}