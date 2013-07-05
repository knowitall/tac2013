package edu.knowitall.tac2013.solr.populate

import org.apache.solr.client.solrj.impl.ConcurrentUpdateSolrServer
import edu.knowitall.tac2013.openie._
import edu.knowitall.tac2013.prep._
import java.util.concurrent.atomic.AtomicInteger
import edu.knowitall.tac2013.prep.util.FileUtils
import scopt.OptionParser
import edu.knowitall.tac2013.prep.util.Line
import edu.knowitall.tac2013.prep.util.LineReader
import edu.knowitall.tac2013.solr.KbpExtractionConverter
import scala.Option.option2Iterable

class SolrPopulator private (val solrServer: ConcurrentUpdateSolrServer) {

  private val errorCounter = new AtomicInteger(0)
  private val extrCounter = new AtomicInteger(0)

  private def tryAddSingle(extr: KbpExtraction): Unit = {
    try {
      addSingle(extr)
    } catch {
      case e: Exception => {
        System.err.println("Error #%d: %s".format(errorCounter.incrementAndGet(), e.getMessage()))
        e.printStackTrace()
      }
    }
  }

  private def addSingle(extr: KbpExtraction): Unit = {
    val solrDoc = KbpExtractionConverter.toSolrInputDocument(extr)
    solrServer.add(solrDoc)
    if (extrCounter.incrementAndGet() % 10000 == 0) {
      System.err.println("SolrPopulator: %d extractions added.".format(extrCounter.get))
    }
  }
  
  private def populate(extrs: Iterator[KbpExtraction]): Unit = {
    
    extrs.grouped(100) foreach { bigGroup =>
      bigGroup.grouped(10).toSeq.par foreach { smallGroup =>
        smallGroup foreach tryAddSingle
      }
    }
    solrServer.commit()
    solrServer.shutdown()
  }
}

object SolrPopulator {
  
  private val queueSize = 1000
  private val threadCount = 4
  
  private def getDefaultSolrServer(solrUrl: String): ConcurrentUpdateSolrServer = {
    new ConcurrentUpdateSolrServer(solrUrl, queueSize, threadCount)
  }
  
  def populate(extrs: Iterator[KbpExtraction], solrUrl: String): Unit = {
    val server = getDefaultSolrServer(solrUrl)
    val populator = new SolrPopulator(server)
    populator.populate(extrs)
  }
  
  def main(args: Array[String]): Unit = {
    
    var inputExtrs = true
    var inputPath = "stdin"
    var solrUrl = "."
    var corpus = "."
    
    val parser = new OptionParser("SolrPopulator") {
      opt("inputPath", "XML or KbpExtraction input file or recursive dir.", { s => inputPath = s})
      arg("solrUrl", "URL to Solr instance.", { s => solrUrl = s})
      opt("inputRaw", "Input is raw XML, not KbpExtractions.", { inputExtrs = false })
      opt("corpus", "For inputRaw, specifies corpus type (news, web, forum.", { s => corpus = s })
    }

    if (!parser.parse(args)) return
    
    val input =  {
      if (inputPath.equals("stdin")) 
        LineReader.stdin
      else {
        val files = FileUtils.getFilesRecursive(new java.io.File(inputPath))
        val readers = files.map(f => LineReader.fromFile(f, "UTF8"))
        FileUtils.getLines(readers)
      }
    }
    val extrs = if (inputExtrs) loadFromKbpExtractions(input) else loadFromXml(input, corpus)
    populate(extrs, solrUrl)
    
  }
  
  def loadFromKbpExtractions(input: Iterator[Line]): Iterator[KbpExtraction] = {
    input.grouped(1000).flatMap { bigGroup => 
      bigGroup.grouped(10).toSeq.par.flatMap { smallGroup =>
        smallGroup flatMap { l => KbpExtraction.read(l.text) }  
      }
    }
  }
  
  def loadFromXml(input: Iterator[Line], corpus: String): Iterator[KbpExtraction] = {
    
    // Load pipeline components
    val docProcessor = KbpDocProcessor.getProcessor(corpus)
    val sentencer = Sentencer.defaultInstance
    val parser = new KbpSentenceParser()
    val extractor = new KbpCombinedExtractor()

    // Move data through the pipe in parallel.
    new DocSplitter(input).grouped(100).flatMap { docs =>
      
      val processedDocs = docs.par flatMap docProcessor.process
      val sentences = processedDocs flatMap sentencer.convertToSentences
      val filteredSentences = sentences flatMap SentenceFilter.apply
      val parsedSentences = filteredSentences flatMap parser.parseKbpSentence
      val extractions = parsedSentences flatMap extractor.extract

      extractions
    }
  }
}