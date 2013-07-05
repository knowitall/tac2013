
package edu.knowitall.tac2013.findSlotFillersApp

import KbpQueryOutput.printUnformattedOutput
import KbpQueryOutput.printFormattedOutput
import KBPQueryEntityType._
import edu.knowitall.tac2013.solr.query.SolrQueryExecutor
import java.io.PrintStream
import scopt.OptionParser

//Command line application object for running solr queries on all the slots
//of a given entity and semantic type
object FindSlotFills {

  def main(args: Array[String]): Unit = {

    var entityName = ""
    var entityType = ""
    var output = System.out
    
    val parser = new OptionParser() {
      arg("entity name", "The entity's name.", { name => entityName = name })
      arg("entity type", "either organization or person.", { typ => entityType = typ })
      opt("outputFile", "File name for output. (default stdout)", { file => output = new PrintStream(file) })
    }
    
    if (!parser.parse(args)) return
      
    println(entityName)
    println(entityType)

    val outputLines = runForServerOutput(entityName, args(1))   
    
    outputLines foreach output.println
    
    if (output != System.out) output.close()
  }

  def runForServerOutput(rawName: String, entityTypeString: String, nodeId: Option[String] = None): Seq[String] = {

    val entityName = rawName.replace("_", " ").trim()
    val entityType = entityTypeString.trim() match {
      case "organization" => ORG
      case "person" => PER
      case _ => throw new IllegalArgumentException("Second Argument must be either 'person' or 'organization'")
    }

    val kbpQuery = KBPQuery.forEntityName(entityName, entityType)
    
    val queryExecutor = SolrQueryExecutor.defaultInstance
    
    val slots = SlotPattern.patternsForQuery(kbpQuery).keySet
    
    val unfilteredSlotCandidateSets = slots.map { slot => (slot, queryExecutor.executeUnfilteredQuery(kbpQuery, slot)) } toMap
    
    val slotCandidateSets = unfilteredSlotCandidateSets map { case (slot, candidates) => 
      (slot -> FilterSolrResults.filterResults(candidates, entityName)) 
    }
    
    val slotBestAnswers = slotCandidateSets map { case (slot, patternCandidates) =>
      (slot -> SlotFillReranker.findAnswers(kbpQuery, patternCandidates))  
    }

    Seq(
      "\n-----------------------------------------\nUNFILTERED RESULTS\n--------------------------------------\n\n",
      printUnformattedOutput(unfilteredSlotCandidateSets, kbpQuery.entityType),
      "\n-----------------------------------------\nFILTERED RESULTS\n--------------------------------------\n\n",
      printUnformattedOutput(slotCandidateSets, kbpQuery.entityType),
      "\n-----------------------------------------\nFORMATTED RESULTS\n--------------------------------------\n\n",
      printFormattedOutput(slotCandidateSets, slotBestAnswers, kbpQuery.entityType))
  }
}