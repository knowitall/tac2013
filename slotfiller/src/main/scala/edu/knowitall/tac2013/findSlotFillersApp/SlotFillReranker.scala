package edu.knowitall.tac2013.findSlotFillersApp

import edu.knowitall.tac2013.openie.KbpExtraction

object SlotFillReranker {
  
  
  def chooseBest(possibleAnswers: List[(KbpSlotToOpenIEData,List[KbpExtraction])]): Option[(KbpSlotToOpenIEData,KbpExtraction)] = {
     var maxConfidence = -1.0
     
     if(possibleAnswers.length > 0){
       
         var bestChoice = Option.empty[(KbpSlotToOpenIEData,KbpExtraction)]
         
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

}