package edu.knowitall.tac2013.findSlotFillersApp

import KbpQueryOutput.printFormattedOutputForKBPQuery
import java.io._
import edu.knowitall.tac2013.solr.query.SolrQueryExecutor

object KBPQueryExecutor {

  def executeKbpQuery(kbpQuery: KBPQuery, outputPath: String) {
    
    val qExec = SolrQueryExecutor.defaultInstance

    val slots = SlotPattern.patternsForQuery(kbpQuery).keySet

    val filteredCandidates = slots map { slot => (slot, qExec.executeQuery(kbpQuery, slot)) } toMap

    val bestAnswers = filteredCandidates map { case (slot, slotCandidates) => 
      (slot, SlotFillReranker.findAnswers(kbpQuery, slotCandidates)) 
    } toMap
    
    printFormattedOutputForKBPQuery(filteredCandidates, bestAnswers, outputPath, kbpQuery)
  }

  def executeKbpQueries(kbpQueryList: List[KBPQuery], outputPath: String) {

    //delete output file before appending to it
    val f = new File(outputPath)
    if (f.exists()) {
      f.delete()
    }

    for (kbpQuery <- kbpQueryList) {
      executeKbpQuery(kbpQuery, outputPath)
    }
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