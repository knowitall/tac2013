package edu.knowitall.tac2013.findSlotFillersApp

class CandidateSet (val pattern: SlotPattern, val candidateExtractions: List[CandidateExtraction]){
   
  val candidateType = { if(candidateExtractions.isEmpty) CandidateType.REGULAR else candidateExtractions(0).candidateType}
  
  

}