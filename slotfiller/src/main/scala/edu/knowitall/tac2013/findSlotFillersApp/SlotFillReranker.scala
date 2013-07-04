package edu.knowitall.tac2013.findSlotFillersApp

import edu.knowitall.tac2013.openie.KbpExtraction

object SlotFillReranker {
  
  
  def chooseBest(possibleAnswers: List[(SlotPattern,List[KbpExtraction])]): Option[(SlotPattern,KbpExtraction)] = {
     var maxConfidence = -1.0
     
     if(possibleAnswers.length > 0){
       
         var bestChoice = Option.empty[(SlotPattern,KbpExtraction)]
         
		 for(ansSet <- possibleAnswers){
		   
		   for(ans <- ansSet._2){
			   if(ans.confidence > maxConfidence){
			     maxConfidence = ans.confidence
			     bestChoice = Some((ansSet._1,ans))
			   }
		   }
		 }
		 
		 if(maxConfidence == -1.0){
		   None
		 }
		 else{
		   bestChoice
		 }
     }
     else{
       
       None
     }
  
    
  }
  
  def chooseBestTest(candidateSets: List[CandidateSet]) : List[CandidateExtraction] = {
    var maxConfidence = -1.0
    var bestChoice = Option.empty[CandidateExtraction]
    for (candidateSet <- candidateSets){
      
      for (extr <- candidateSet.candidateExtractions){
        if(extr.kbpExtraction.confidence > maxConfidence){
          maxConfidence = extr.kbpExtraction.confidence
          bestChoice = Some(extr)
        }
        
      }
 
    }
    
    if(maxConfidence == -1.0){
      List.empty[CandidateExtraction]
    }
    else{
      List(bestChoice.get)
    }
  }
}