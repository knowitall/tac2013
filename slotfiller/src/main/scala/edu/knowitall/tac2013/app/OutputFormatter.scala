package edu.knowitall.tac2013.app

import scala.io._
import java.io._
import edu.knowitall.tac2013.solr.query.SolrQuery
import edu.knowitall.tac2013.openie.KbpExtraction
import edu.knowitall.tac2013.app.KBPQueryEntityType._
import edu.knowitall.collection.immutable.Interval


case class OutputFormatter(
    val out: PrintStream, 
    val printUnfiltered: Boolean = false, 
    val printFiltered: Boolean = false, 
    val detailedCandidates: Boolean = true,
    val printGroups: Boolean = true,
    val detailedGroups: Boolean = true,
    val printAnswers: Boolean = true,
    val detailedAnswers: Boolean = true,
    val runID: String = "UWashington_1") {
  
   
  val indentSize = 4
  
  val maxUnformatted = 12
  
  val doubleSpace = false
  
  val maxGroups = 15
  
  val indentStr: String = Seq.fill(indentSize)(' ').mkString
  
  private def indentStr(i: Int): String = Seq.fill(i)(indentStr).mkString
    
  private def println(i: Int, string: String) = {
    out.println(s"${indentStr(i)}$string")
    if (doubleSpace) out.println
  }
    
  private def printSolrResults(resultsName: String, mapOfResults: Map[Slot, Seq[Candidate]], kbpQuery: KBPQuery): Unit = {

    println(0, "")
    println(0, s"------------------- $resultsName RESULTS -------------------")
    println(0, "")
    
    for (kbpSlot <- kbpQuery.slotsToFill) {
      printSlotOutput(kbpSlot, mapOfResults(kbpSlot))
    }
  }
  
  def printFilteredResults(mapOfResults: Map[Slot, Seq[Candidate]], kbpQuery: KBPQuery): Unit = {
    if (printFiltered) printSolrResults("FILTERED", mapOfResults, kbpQuery)
  }
  
  def printUnfilteredResults(mapOfResults: Map[Slot, Seq[Candidate]], kbpQuery: KBPQuery): Unit = { 
    if (printUnfiltered) printSolrResults("UNFILTERED", mapOfResults, kbpQuery)
  }
  
  private def printSlotOutput(slot: Slot, slotCandidates: Seq[Candidate]): Unit = {

    require(slotCandidates.forall(_.pattern.slotName.equals(slot.name)))

    println(0, s"(${slotCandidates.size}) ${slot.name}")

    val patternCandidatesMap = slotCandidates.groupBy(_.pattern)
    if (slotCandidates.size > 0) for (
      pattern <- slot.patterns;
      patternCandidates = patternCandidatesMap.getOrElse(pattern, Nil)
    ) {

      printPatternOutput(pattern, patternCandidates)
    }
    println(0, "")
  }

  private def printPatternOutput(pattern: SlotPattern, patternCandidates: Seq[Candidate]): Unit = {
    val queryCandidatesMap = patternCandidates.groupBy(_.solrQuery)
    println(1, "(%d) ".format(patternCandidates.size) + pattern.debugString)
    for ((solrQuery, queryCandidates) <- queryCandidatesMap) {

      printQueryOutput(solrQuery, queryCandidates)
    }
  }

  private def printQueryOutput(solrQuery: SolrQuery, queryCandidates: Seq[Candidate]): Unit = {
    val topCandidates = queryCandidates.take(maxUnformatted)
    val numTruncated = queryCandidates.size - maxUnformatted
    println(2, "(%d) ".format(queryCandidates.size) + solrQuery.queryString)

    if (detailedCandidates) {
      for (candidate <- topCandidates) {
        println(3, candidate.debugString)
      }
      if (numTruncated > 0) println(3, "(%d more)".format(numTruncated))
    }
  }
  
  /**
   * Overloaded to return a string for server usage
   */
  def printAnswers(bestAnswers: Map[Slot, Seq[Candidate]], kbpQuery: KBPQuery): Unit = if (printAnswers) {

    val detailed = if (detailedAnswers) "DEBUG " else ""

    if (detailedAnswers) {
      println(0, "")
      println(0, s"------------------- $detailed FORMATTED SLOT FILLS -------------------")
      println(0, "")
    }
    
    //iterate over every slot type
    for (kbpSlot <- kbpQuery.slotsToFill) yield {
      if (detailedAnswers) printDetailedSlotAnswer(kbpSlot, kbpQuery, bestAnswers.getOrElse(kbpSlot, Nil))
      else printSlotAnswer(kbpSlot, kbpQuery, bestAnswers.getOrElse(kbpSlot, Nil))
    }
  }

  private def printSlotAnswer(slot: Slot, kbpQuery: KBPQuery, bestAnswers: Seq[Candidate]): Unit = {

    if (bestAnswers.isEmpty) {
      out.println(Iterator(kbpQuery.id, slot.name, runID, "NIL").mkString("\t"))
    } else {
      for (bestAnswer <- bestAnswers) {
        val queryData = bestAnswer.pattern
        val bestExtr = bestAnswer.extr
        val slotFillIn = queryData.slotFillIn.get.toLowerCase()
        
        require(slotFillIn == "arg1" || slotFillIn == "arg2" || slotFillIn =="relation")
        
        val fields = Iterator(
          kbpQuery.id,
          slot.name,
          runID,
          bestAnswer.extr.sentence.docId,
          bestAnswer.trimmedFill.string,
          bestAnswer.fillOffsetString,
          bestAnswer.entityOffsetString,
          bestAnswer.justificationOffsetString,
          bestAnswer.extr.confidence)
        
        out.println(fields.mkString("\t"))
      }
    }
  }
  
  private def printDetailedSlotAnswer(slot: Slot, kbpQuery: KBPQuery, bestAnswers: Seq[Candidate]): Unit = {
    
    if (bestAnswers.isEmpty) {
      out.println(Iterator(kbpQuery.id, slot.name, runID, "NIL").mkString("\t"))
    } else {
      for (bestAnswer <- bestAnswers) {
        val queryData = bestAnswer.pattern
        val bestExtr = bestAnswer.extr
        val slotFillIn = queryData.slotFillIn.get.toLowerCase()
        
        require(slotFillIn == "arg1" || slotFillIn == "arg2" || slotFillIn =="relation")
        
        val fields = Iterator(
          kbpQuery.id,
          slot.name,
          runID,
          bestAnswer.extr.sentence.docId,
          "Fill: " + bestAnswer.trimmedFill.string + " " +bestAnswer.fillOffsetString,
          "Entity: " + bestAnswer.trimmedEntity.string + " " + bestAnswer.entityOffsetString,
          "Just: " + bestAnswer.extr.sentence.dgraph.text + " " +bestAnswer.justificationOffsetString,
          bestAnswer.extr.confidence)
        
        out.println(fields.mkString("\t"))
      }
    }
  }
  
  def printFillGroups(header: String, slot: Slot, groups: Map[String, Seq[Candidate]]): Unit = if (printGroups) {
    
    println(0, "")
    println(0, s"------------------- GROUPS: ${slot.name} $header -------------------")
    println(0, "")
    
    val sortedGroups = groups.iterator.toSeq.sortBy(-_._2.size)
    val truncatedGroups = sortedGroups.take(maxGroups)
    val numTruncated = groups.keys.size - maxGroups
    val groupScore = groups map { case (key, candidates) => (key, "%.02f".format(Candidate.groupScore(candidates))) }
    // the keys we actually display with size info
    val displayKeys = groups map { case (key, candidates) => (key, s"$key(${groupScore(key)},${candidates.size})") }
    
    val maxKeyLength = displayKeys.values.map(_.length).max  
    val pad: String = Seq.fill(maxKeyLength + 1)(' ').mkString
    def padStr(str: String): String = str + Seq.fill(maxKeyLength - str.length + 1)(' ').mkString

    if (detailedGroups) {
      truncatedGroups.foreach {
        case (key, candidates) =>
          val displayKey = displayKeys(key)
          println(0, padStr(displayKey) + candidates.head.debugString)
          candidates.tail.foreach { candidate =>
            println(0, pad + candidate.debugString)
          }
          println(0, "")
      }
      
    } else {
      truncatedGroups.foreach { case (key, candidates) =>
        val displayKey = displayKeys(key)
        println(0, "%s (%d)".format(padStr(displayKey), candidates.size))
      }
    }
    if (numTruncated > 0) println(0, s"($numTruncated groups truncated)")
  }
}

object OutputFormatter {
  val default = new OutputFormatter(System.out) 
  
  def detailedAnswersOnly(output: PrintStream, runIDName: String) = 
    new OutputFormatter(output, printUnfiltered = false, printFiltered = false, printGroups = false, printAnswers = true, detailedAnswers = true, runID = runIDName)
  
  def formattedAnswersOnly(output: PrintStream, runIDName: String) = 
    new OutputFormatter(output, printUnfiltered = false, printFiltered = false, printGroups = false, printAnswers = true, detailedAnswers = false, runID = runIDName)
  
}
