
package edu.knowitall.tac2013.app

import KBPQueryEntityType._
import edu.knowitall.tac2013.solr.query.SolrQueryExecutor
import java.io.PrintStream
import scopt.OptionParser

//Command line application object for running solr queries on all the slots
//of a given entity and semantic type
class FindSlotFills(val queryExecutor: SolrQueryExecutor) {
  def this(url: String) = this(new SolrQueryExecutor(url))
  
  def main(args: Array[String]): Unit = {

    var entityName = ""
    var entityType = ""
    var slotStrings = ""
    var output = System.out
    
    val parser = new OptionParser() {
      arg("entity name", "The entity's name.", { name => entityName = name })
      arg("entity type", "either organization or person.", { typ => entityType = typ })
      opt("slots", "Specific slots to fill, default = all, comma separated", { slotStrings = _ })
      opt("outputFile", "File name for output. (default stdout)", { file => output = new PrintStream(file) })
    }
    
    if (!parser.parse(args)) return
      
    println(entityName)
    println(entityType)

    val slots = slotStrings.split(",").map(_.trim).filter(_.nonEmpty).toSet
    
    runForServerOutput(entityName, None, entityType, slots, new OutputFormatter(System.out))
    
    if (output != System.out) output.close()
  }

  def runForServerOutput(rawName: String, nodeId: Option[String], entityTypeString: String, overrideSlotNames: Set[String], fmt: OutputFormatter): Unit = {
    
    val entityName = rawName.replace("_", " ").trim()
    val entityType = entityTypeString.trim() match {
      case "organization" => ORG
      case "person" => PER
      case _ => throw new IllegalArgumentException("Second Argument must be either 'person' or 'organization'")
    }
    val overrideSlots = overrideSlotNames map Slot.fromName

    val kbpQuery = if (overrideSlotNames.isEmpty) {
      KBPQuery.forEntityName(entityName, entityType, nodeId)
    } else {
      KBPQuery.forEntityName(entityName, entityType, nodeId).withOverrideSlots(overrideSlots)
    }
    
    val unfilteredSlotCandidateSets = kbpQuery.slotsToFill.map { slot => 
      (slot, queryExecutor.executeUnfilteredQuery(kbpQuery, slot)) 
    } toMap
    
    
    fmt.printUnfilteredResults(unfilteredSlotCandidateSets, kbpQuery)
    
    val slotCandidateSets = unfilteredSlotCandidateSets map { case (slot, candidates) => 
      (slot -> FilterSolrResults.filterResults(candidates, entityName)) 
    }
    
    fmt.printFilteredResults(slotCandidateSets, kbpQuery)
    
    val slotBestAnswers = slotCandidateSets map { case (slot, patternCandidates) =>
      (slot -> new SlotFillReranker(fmt).findSlotAnswers(slot, kbpQuery, patternCandidates))  
    }
    
    val smoothedSlotBestAnswers = SlotFillConsistency.makeConsistent(slotBestAnswers)
    
    fmt.printAnswers(smoothedSlotBestAnswers, kbpQuery)
  }
}