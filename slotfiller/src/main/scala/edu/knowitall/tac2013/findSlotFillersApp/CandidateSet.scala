package edu.knowitall.tac2013.findSlotFillersApp

class CandidateSet (val pattern: KbpSlotToOpenIEData, val candidateExtractions: List[CandidateExtraction]){
   
  val candidateType = { if(candidateExtractions.isEmpty) CandidateType.REGULAR else candidateExtractions(0).candidateType}
  
  

}