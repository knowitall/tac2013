
package edu.knowitall.tac2013.app

import KBPQueryEntityType._
import edu.knowitall.tac2013.solr.query.SolrQueryExecutor
import java.io.PrintStream
import scopt.OptionParser
import edu.knowitall.tac2013.app.util.DocUtils
import edu.knowitall.tac2013.solr.query.SolrHelper

//Command line application object for running solr queries on all the slots
//of a given entity and semantic type
class FindSlotFills(val oldOrNew: String, val corefOn: Boolean) {
  
  val queryExecutor = SolrQueryExecutor.getInstance(oldOrNew, corefOn)
  
  //set XML url to correct place
  SolrHelper.setConfigurations(oldOrNew,corefOn)
  
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
    
    runForServerOutput(entityName, None, entityType, slots, new OutputFormatter(System.out), Nil)
    
    if (output != System.out) output.close()
  }

  
  val splitPattern = ",|\t".r
  
  def runForServerOutput(
      rawName: String, 
      nodeId: Option[String], 
      entityTypeString: String, 
      overrideSlotNames: Set[String], 
      fmt: OutputFormatter, 
      extraPatternStrings: Seq[String]): Unit = {
    
    val entityName = rawName.replace("_", " ").trim()
    val entityType = KBPQueryEntityType.fromString(entityTypeString)
    val overrideSlots = overrideSlotNames map Slot.fromName
    
    val extraPatternOpts = extraPatternStrings.map(s => splitPattern.split(s).map(_.trim)).map(split => (split, SlotPattern.read(split)))
    val extraPatterns = extraPatternOpts.flatMap { case (split, patOpt) =>
      patOpt match {
        case Some(pat) => {
          fmt.out.println("Including extra pattern: " + split.mkString(",") + " => " + pat.debugString)
          Some(pat)
        }
        case None => {
          fmt.out.println("Couldn't parse pattern: " + split.mkString(","))
          None
        }
      }
    }

    val kbpQuery = if (overrideSlotNames.isEmpty) {
      KBPQuery.forEntityName(entityName, entityType, nodeId, extraPatterns)
    } else {
      KBPQuery.forEntityName(entityName, entityType, nodeId, extraPatterns).withOverrideSlots(Slot.addPatterns(overrideSlots, extraPatterns))
    }
    
    val unfilteredSlotCandidateSets = kbpQuery.slotsToFill.map { slot => 
      (slot, queryExecutor.executeUnfilteredQuery(kbpQuery, slot)) 
    } toMap
    
    
    fmt.printUnfilteredResults(unfilteredSlotCandidateSets, kbpQuery)
    
    val slotCandidateSets = unfilteredSlotCandidateSets map { case (slot, candidates) => 
      (slot -> FilterSolrResults.filterResults(candidates, kbpQuery)) 
    }
    
    fmt.printFilteredResults(slotCandidateSets, kbpQuery)
    
    //get correct date format strings
    DocUtils.putInTimexFormat(slotCandidateSets)
    DocUtils.findBestFillMention(slotCandidateSets)
    
    val slotBestAnswers = slotCandidateSets map { case (slot, patternCandidates) =>
      (slot -> new SlotFillReranker(fmt).findSlotAnswers(slot, kbpQuery, patternCandidates))  
    }
    
    val smoothedSlotBestAnswers = SlotFillConsistency.makeConsistent(slotBestAnswers)
    
    fmt.printAnswers(smoothedSlotBestAnswers, kbpQuery)
  }
}