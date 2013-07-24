package edu.knowitall.tac2013.app

import java.io._
import edu.knowitall.tac2013.solr.query.SolrQueryExecutor
import edu.knowitall.tac2013.app.FilterSolrResults.filterResults
import edu.knowitall.tac2013.app.util.DocUtils
import edu.knowitall.tac2013.solr.query.SolrHelper
import scopt.OptionParser

object KBPQueryExecutor {

  def executeKbpQuery(queryExecutor: SolrQueryExecutor, kbpQuery: KBPQuery, outFmt: OutputFormatter, oldOrNew: String): Unit = {

    val slots = kbpQuery.slotsToFill

    val unfiltered = slots map { slot => (slot, queryExecutor.executeUnfilteredQuery(kbpQuery, slot)) } toMap
    
    val filteredCandidates = slots map { slot => (slot, filterResults(unfiltered(slot), kbpQuery)) } toMap
    
    DocUtils.putInTimexFormat(filteredCandidates,oldOrNew)

    val bestAnswers = slots map { slot => 
      (slot, new SlotFillReranker(outFmt).findSlotAnswers(slot, kbpQuery, filteredCandidates(slot))) 
    } toMap
    
    val smoothedSlotBestAnswers = SlotFillConsistency.makeConsistent(bestAnswers)
    
    outFmt.printAnswers(smoothedSlotBestAnswers, kbpQuery)
  }

  def main(args: Array[String]) {

    var queryFile = "."
    var outputFile = "."
    var corpus = "old"
    var detailed = false
    var corefOn = false
    var runID = "UWashington_1"
    
    val parser = new OptionParser() {
      arg("queryFile", "Path to query file.", { s => queryFile = s })
      arg("outputFile", "Path to output file.", { s => outputFile = s })
      arg("corpus", "Either \"old\" or \"new\".", { s => corpus = s })
      opt("detailed", "Produce more verbose output", { detailed = true })
      opt("coref", "Turn on coref module", { corefOn = true })
      opt("runID", "Set runID name", {s => runID = s})
    }
    
    if (!parser.parse(args)) return

    val queryExecutor = SolrQueryExecutor.getInstance(corpus, corefOn)
    //set configs for SolrHelper
    SolrHelper.setConfigurations(corpus, corefOn)
    
    val outputStream = new PrintStream(outputFile)
    val outputFormatter = detailed match {
      case true => OutputFormatter.detailedAnswersOnly(outputStream,runID)
      case false => OutputFormatter.formattedAnswersOnly(outputStream,runID)
    }

    val kbpQueryList = KBPQuery.parseKBPQueries(queryFile)
        for (kbpQuery <- kbpQueryList) {
          try{
             executeKbpQuery(queryExecutor, kbpQuery, outputFormatter,corpus)
             //clear hashmaps from previous queries in stanford helper so we don't
             //get memory issues 
             DocUtils.stanfordHelper.clearHashMaps()
          }
          //catch any exception and print nil for every slot in the query that failed
          catch{
            case e: Exception => {
              outputFormatter.printEmpty(kbpQuery)
            }
          }
    }
    
    outputStream.close()
  }
}