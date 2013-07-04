
package edu.knowitall.tac2013.findSlotFillersApp

import QueryEntityForAllSlots.executeQueryForAllSlots
import QueryEntityForAllSlots.executeQueryForAllSlotsWithoutFilter
import KbpQueryOutput.printUnformattedOutput
import KbpQueryOutput.printFormattedOutput
import SlotFillReranker.chooseBestTest
import KBPQueryEntityType._

//Command line application object for running solr queries on all the slots
//of a given entity and semantic type
object FindSlotFills {

  def main(args: Array[String]) {

    assert(args.length == 3,
      "there should be three arguments: entity name, semantic type (organization or person), and file path for output")

    val entityName = args(0).replace("_", " ")
    val entityType = args(1) match {
      case "organization" => ORG
      case "person" => PER
      case _ => throw new IllegalArgumentException("Second Argument must be either 'person' or 'organization'")
    }
    println(entityName)
    println(args(1))

    val kbpQuery = KBPQuery.forEntityName(entityName, entityType)

    val mapOfResults = executeQueryForAllSlots(kbpQuery, SlotPattern.patternsForQuery(kbpQuery))

    // rank candidate extractions and build a map from slot names to SlotCandidateSet
    var slotCandidateSetMap = Map[String, SlotCandidateSet]()
    for (x <- mapOfResults.keys) {
      slotCandidateSetMap += (x -> new SlotCandidateSet(entityName, mapOfResults(x)));
      slotCandidateSetMap(x).setRankedAnswers(chooseBestTest(slotCandidateSetMap(x).candidateSets));
    }

    printUnformattedOutput(slotCandidateSetMap, args(2), kbpQuery.entityType)
    printFormattedOutput(slotCandidateSetMap, args(2), kbpQuery.entityType)
  }

  def runForServerOutput(field1: String, field2: String, nodeId: Option[String] = None): String = {

    val entityName = field1.replace("_", " ").trim()
    val entityType = field2.trim() match {
      case "organization" => ORG
      case "person" => PER
      case _ => throw new IllegalArgumentException("Second Argument must be either 'person' or 'organization'")
    }

    val kbpQuery = KBPQuery.forEntityName(entityName, entityType)
    
    val patterns = SlotPattern.patternsForQuery(kbpQuery)

    val mapOfResults = executeQueryForAllSlots(kbpQuery, patterns)

    val unfilteredMapOfResults = executeQueryForAllSlotsWithoutFilter(kbpQuery, patterns)

    //build a map from slot names to SlotCandidateSet
    var filteredSlotCandidateSetMap = Map[String, SlotCandidateSet]()
    for (x <- mapOfResults.keys) {
      filteredSlotCandidateSetMap += (x -> new SlotCandidateSet(entityName, mapOfResults(x)))
      filteredSlotCandidateSetMap(x).setRankedAnswers(chooseBestTest(filteredSlotCandidateSetMap(x).candidateSets));
    }
    var unFilteredSlotCandidateSetMap = Map[String, SlotCandidateSet]()
    for (x <- unfilteredMapOfResults.keys) {
      unFilteredSlotCandidateSetMap += (x -> new SlotCandidateSet(entityName, unfilteredMapOfResults(x)))
    }

    return (
      "\n-----------------------------------------\nUNFILTERED RESULTS\n--------------------------------------\n\n" +
      printUnformattedOutput(unFilteredSlotCandidateSetMap, kbpQuery.entityType) +
      "\n-----------------------------------------\nFILTERED RESULTS\n--------------------------------------\n\n" +
      printUnformattedOutput(filteredSlotCandidateSetMap, kbpQuery.entityType) +
      "\n-----------------------------------------\nFORMATTED RESULTS\n--------------------------------------\n\n" +
      printFormattedOutput(filteredSlotCandidateSetMap, kbpQuery.entityType))
  }
}