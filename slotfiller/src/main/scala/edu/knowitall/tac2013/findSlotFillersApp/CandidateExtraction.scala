package edu.knowitall.tac2013.findSlotFillersApp

import edu.knowitall.tac2013.openie.KbpExtraction

class CandidateExtraction (val kbpExtraction: KbpExtraction, val candidateType: CandidateType.Value, val pattern: SlotPattern) {
  
  def getEntityField = pattern.entityIn match {
    case Some("arg1") => kbpExtraction.arg1
    case Some("rel") => kbpExtraction.rel
    case Some("arg2") => kbpExtraction.arg2
    case _ => throw new RuntimeException("Unknown entity field")
  }
}