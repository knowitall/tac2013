package edu.knowitall.tac2013.app

import java.io._
import edu.knowitall.tac2013.solr.query.SolrQueryExecutor
import edu.knowitall.tac2013.app.FilterSolrResults.filterResults

object KBPQueryExecutor {

  def executeKbpQuery(kbpQuery: KBPQuery, outFmt: OutputFormatter): Unit = {
    
    val qExec = SolrQueryExecutor.defaultInstance

    val slots = kbpQuery.slotsToFill

    val unfiltered = slots map { slot => (slot, qExec.executeUnfilteredQuery(kbpQuery, slot)) } toMap
    
    val filteredCandidates = slots map { slot => (slot, filterResults(unfiltered(slot), kbpQuery.name)) } toMap

    val bestAnswers = slots map { slot => 
      (slot, new SlotFillReranker(outFmt).findSlotAnswers(slot, kbpQuery, filteredCandidates(slot))) 
    } toMap
    
    val smoothedSlotBestAnswers = SlotFillConsistency.makeConsistent(bestAnswers)
    
    outFmt.printAnswers(smoothedSlotBestAnswers, kbpQuery)
  }

  def executeKbpQueries(kbpQueryList: List[KBPQuery], outputPath: String) {

    //delete output file before appending to it
    val f = new File(outputPath)
    if (f.exists()) {
      f.delete()
    }

    val output = new PrintStream(outputPath)

    val outFmt = new OutputFormatter(output)
    
    for (kbpQuery <- kbpQueryList) {
      executeKbpQuery(kbpQuery, outFmt)
    }
    output.close()
  }

  def main(args: Array[String]) {

    assert(args.length == 2,
      "there should be two arguments: path to KBP query File, path to the output File")

    val KBPQueryPath = args(0)
    val outputPath = args(1)

    val kbpQueryList = KBPQuery.parseKBPQueries(KBPQueryPath)
    executeKbpQueries(kbpQueryList, outputPath)
  }
}