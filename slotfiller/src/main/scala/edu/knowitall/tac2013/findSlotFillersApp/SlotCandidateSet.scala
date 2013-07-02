package edu.knowitall.tac2013.findSlotFillersApp

class SlotCandidateSet (val slot: String, val candidateSets: List[CandidateSet]){
  
  var rankedAnswers = List[CandidateExtraction]()
  
  
  def setRankedAnswers(newRankedAnswers: List[CandidateExtraction]){rankedAnswers = newRankedAnswers}
  

}