package edu.knowitall.tac2013.findSlotFillersApp

import scala.io._
import java.io._
import edu.knowitall.tac2013.openie.KbpExtraction
import edu.knowitall.tac2013.findSlotFillersApp.KBPQueryEntityType._
import edu.knowitall.collection.immutable.Interval

object KbpQueryOutput {

  val runID = "UWashington-1"

  def printUnformattedOutput(mapOfResults: Map[String, Seq[Candidate]], kbpQueryEntityType: KBPQueryEntityType): String = {

    val slotOutputs = for (kbpSlot <- SlotTypes.getSlotTypesList(kbpQueryEntityType)) yield {
      printUnformattedSlotOutput(kbpSlot, mapOfResults(kbpSlot))
    }
    slotOutputs.mkString
  }

  private def printUnformattedSlotOutput(slot: String, slotCandidates: Seq[Candidate]): String = {

    require(slotCandidates.forall(_.pattern.slotName.equals(slot)))

    if (slotCandidates.isEmpty) {
      s"KBP SLOT NAME: $slot\n\tNil\n"
    } else {

      val sb = new StringBuilder
      sb.append(s"KBP SLOT NAME: $slot\n")

      val patternCandidates = slotCandidates.groupBy(_.pattern)
      for ((pattern, candidates) <- patternCandidates) {
        val topCandidates = candidates.take(20)

        sb.append("\tQuery pattern:\t" + pattern.debugString)

        sb.append("\tResults:\n")
        if (topCandidates.length == 0) {
          sb.append("\t\tNil" + "\n")
        } else {
          for (candidate <- topCandidates) {
            sb.append("\t\targ1: " + candidate.extr.arg1.originalText + "\t rel: " + candidate.extr.rel.originalText +
              "\t arg2: " + candidate.extr.arg2.originalText + "\t docID: " + candidate.extr.sentence.docId +
              "\t confidence: " + candidate.extr.confidence + "\t sentence: " + candidate.extr.sentence.dgraph.text  + 
               "\t trimFill: " + candidate.trimmedFill.trimmedFillString + "\n" )
          }
          sb.append("\n")
        }
      }
      sb.toString
    }
  }

  /**
   * Overloaded to return a string for server usage
   */
  def printFormattedOutput(
    slotCandidateSets: Map[String, Seq[Candidate]],
    bestAnswers: Map[String, Seq[Candidate]],
    kbpQueryEntityType: KBPQueryEntityType): String = {

    //iterate over every slot type
    val slotOutputs = for (kbpSlot <- SlotTypes.getSlotTypesList(kbpQueryEntityType)) yield {
      if (slotCandidateSets.contains(kbpSlot)) {

        // for each slot print one response for single-valued slot
        // print k-slots for multi-valued slot
        // or print NIL
        printFormattedSlotOutput(kbpSlot, bestAnswers(kbpSlot))
      } else {
        // else if the results Map does not contain the slot
        // print nothing since this slot is ignored
        ""
      }
    }
    slotOutputs.mkString
  }

  def printFormattedSlotOutput(kbpSlot: String, bestAnswers: Seq[Candidate]): String = {

    val sb = new StringBuilder

    if (bestAnswers.isEmpty) {
      sb.append(Iterator("queryID", kbpSlot, "runID", "NIL").mkString("\t") + "\n")
    } else {
      for (bestAnswer <- bestAnswers) {
        val queryData = bestAnswer.pattern
        val bestExtr = bestAnswer.extr
        val slotFillIn = queryData.slotFillIn.get.toLowerCase()
        
        require(slotFillIn == "arg1" || slotFillIn == "arg2")
        
        val fields = Iterator(
          "queryID",
          kbpSlot,
          "runID",
          bestAnswer.extr.sentence.docId,
          bestAnswer.trimmedFill.trimmedFillString,
          bestAnswer.fillOffsetString,
          bestAnswer.entityOffsetString,
          bestAnswer.relOffsetString,
          bestAnswer.extr.confidence)
        
        sb.append(fields.mkString("\t") + "\n")
      }
    }
    sb.toString()
  }

  def printFormattedOutputForKBPQuery(
    slotCandidateSets: Map[String, Seq[Candidate]],
    bestAnswers: Map[String, Seq[Candidate]],
    filePath: String, kbpQuery: KBPQuery) {

    val writer = new FileWriter(new File(filePath), true)

    //iterate over every slot type
    for (kbpSlot <- SlotTypes.getSlotTypesList(kbpQuery.entityType)) {

      //if the kbp slot is contained in the results
      if (slotCandidateSets.contains(kbpSlot)) {

        //for each slot print one response for single-valued slot
        //print k-slots for multi-valued slot
        //or print NIL

        val kbpSlotName = kbpSlot

        //for now assume every slot is single valued, use a
        //separate filter method to choose best answer

        val bestAnswerExtractions = bestAnswers(kbpSlot)

        if (!bestAnswerExtractions.isEmpty) {
          val bestAnswer = bestAnswerExtractions.head
          val queryData = bestAnswer.pattern
          val slotFiller = {
            if (queryData.slotFillIn.get.toLowerCase().trim() == "arg1") {
              bestAnswer.extr.arg1.originalText
            } else if (queryData.slotFillIn.get.toLowerCase().trim() == "arg2") {
              bestAnswer.extr.arg2.originalText
            }
          }

          val fillerOffsetInterval = {
            if (queryData.slotFillIn.get.toLowerCase().trim() == "arg1") {
              bestAnswer.extr.arg1.tokenInterval
            } else if (queryData.slotFillIn.get.toLowerCase().trim() == "arg2") {
              bestAnswer.extr.arg2.tokenInterval
            } else if (queryData.slotFillIn.get.toLowerCase().trim() == "relation") {
              bestAnswer.extr.rel.tokenInterval
            } else {
              throw new Exception("slotFillIn specification should only be arg1, arg2, or relation")
            }
          }

          val entityOffsetInterval = {
            if (queryData.entityIn.get.toLowerCase().trim() == "arg1") {
              bestAnswer.extr.arg1.tokenInterval
            } else if (queryData.entityIn.get.toLowerCase().trim() == "arg2") {
              bestAnswer.extr.arg2.tokenInterval
            } else {
              throw new Exception("entityIn specification should only be arg1 or arg2")
            }
          }

          val startOffset = bestAnswer.extr.sentence.startOffset

          val fillerOffsetStart = startOffset + fillerOffsetInterval.start
          val fillerOffsetEnd = startOffset + fillerOffsetInterval.end
          val fillerOffsetString = fillerOffsetStart.toString + "-" + fillerOffsetEnd.toString()

          val entityOffsetStart = startOffset + entityOffsetInterval.start
          val entityOffsetEnd = startOffset + entityOffsetInterval.end
          val entityOffsetString = entityOffsetStart.toString + "-" + entityOffsetEnd.toString()

          val justificationOffsetStart = startOffset + bestAnswer.extr.arg1.tokenInterval.start
          val justificationOffsetEnd = startOffset + bestAnswer.extr.arg2.tokenInterval.end
          val justificationOffsetString = justificationOffsetStart.toString() + "-" + justificationOffsetEnd.toString()

          writer.write(Iterator(kbpQuery.id, kbpSlot, runID, bestAnswer.extr.sentence.docId, slotFiller,
            fillerOffsetString, entityOffsetString, justificationOffsetString, bestAnswer.extr.confidence).mkString("\t") + "\n")

        } else {
          //no answer found
          writer.write(Iterator(kbpQuery.id, kbpSlot, runID, "NIL").mkString("\t") + "\n")
        }
      } else {
        // else if the results Map does not contain the slot
        // print nothing since this slot is ignored
      }
    }
    writer.close()
  }
}