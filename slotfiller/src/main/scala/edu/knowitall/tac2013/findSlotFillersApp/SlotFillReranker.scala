package edu.knowitall.tac2013.findSlotFillersApp

import edu.knowitall.tac2013.openie.KbpExtraction

/**
 * Finds fills given candidates for a particular slot.
 */
object SlotFillReranker {

  /*
   * Requires that all candidates (if any) are for the same slot.
   */
  def findAnswers(kbpQuery: KBPQuery, slotCandidates: Seq[Candidate]): Seq[Candidate] = {
    
    if (slotCandidates.isEmpty) List.empty
    
    else {
      
      val slot = slotCandidates.head.pattern.slotName
      val maxAnswers = slotCandidates.head.pattern.maxValues
      
      require(maxAnswers.isDefined)
      require(slotCandidates.forall(_.pattern.slotName.equals(slot)))
      require(slotCandidates.forall(_.pattern.maxValues == maxAnswers))
      
      // group results by their tuple key
      val groups = slotCandidates.groupBy(_.extractionKey)
      
      // rank best result from each group according to a confidence measure
      // in descending order
      val bestResults = groups.values.flatMap({ extractions =>
        extractions.headOption map { headExtr =>
          (headExtr, headExtr.extr.confidence + (extractions.size  * 0.1))
        }
      }).toSeq.sortBy(-_._2)
      
      bestResults.map(_._1).take(maxAnswers.get)
    }
  }
}