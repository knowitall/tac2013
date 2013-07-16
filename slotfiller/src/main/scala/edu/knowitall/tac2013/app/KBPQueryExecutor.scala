package edu.knowitall.tac2013.app

import java.io._
import edu.knowitall.tac2013.solr.query.SolrQueryExecutor
import edu.knowitall.tac2013.app.FilterSolrResults.filterResults

object KBPQueryExecutor {

  def executeKbpQuery(queryExecutor: SolrQueryExecutor, kbpQuery: KBPQuery, outFmt: OutputFormatter): Unit = {

    val slots = kbpQuery.slotsToFill

    val unfiltered = slots map { slot => (slot, queryExecutor.executeUnfilteredQuery(kbpQuery, slot)) } toMap
    
    val filteredCandidates = slots map { slot => (slot, filterResults(unfiltered(slot), kbpQuery)) } toMap

    val bestAnswers = slots map { slot => 
      (slot, new SlotFillReranker(outFmt).findSlotAnswers(slot, kbpQuery, filteredCandidates(slot))) 
    } toMap
    
    val smoothedSlotBestAnswers = SlotFillConsistency.makeConsistent(bestAnswers)
    
    outFmt.printAnswers(smoothedSlotBestAnswers, kbpQuery)
  }

  def executeKbpQueries(queryExecutor: SolrQueryExecutor, kbpQueryList: List[KBPQuery], outputPath: String) {

    //delete output file before appending to it
    val f = new File(outputPath)
    if (f.exists()) {
      f.delete()
    }

    val output = new PrintStream(outputPath)

    val outFmt = OutputFormatter.detailedAnswersOnly(output)
    
    for (kbpQuery <- kbpQueryList) {
      executeKbpQuery(queryExecutor, kbpQuery, outFmt)
    }
    output.close()
  }

  def main(args: Array[String]) {

    assert(args.length == 3,
      "there should be three arguments: path to KBP query File, path to the output File, and \"old\" or \"new\" to specify corpus")

    val queryExecutor = SolrQueryExecutor.getInstance(args(2))
    val KBPQueryPath = args(0)
    val outputPath = args(1)

    val kbpQueryList = KBPQuery.parseKBPQueries(KBPQueryPath)
    executeKbpQueries(queryExecutor, kbpQueryList, outputPath)
  }
}