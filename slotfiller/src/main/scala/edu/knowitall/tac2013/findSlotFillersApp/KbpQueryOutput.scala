package edu.knowitall.tac2013.findSlotFillersApp

import scala.io._
import java.io._
import edu.knowitall.tac2013.openie.KbpExtraction
import edu.knowitall.tac2013.findSlotFillersApp.KBPQueryEntityType._
import edu.knowitall.tac2013.findSlotFillersApp.SlotFillReranker.chooseBest
import edu.knowitall.collection.immutable.Interval

object KbpQueryOutput {
  
  val runID = "UWashington-1"
  
  def printUnformattedOutput(mapOfResults: Map[String,SlotCandidateSet], filePath: String, kbpQueryEntityType: KBPQueryEntityType){
    
    val writer = new PrintWriter(new File(filePath))
    //iterate over all possible slot names
    for(kbpSlot <- SlotTypes.getSlotTypesList(kbpQueryEntityType)){
      
      if(mapOfResults.contains(kbpSlot)){
	      val kbpSlotName = kbpSlot
	      val slotCandidateSet = mapOfResults(kbpSlot)
	      
	      //only return up to 20 solr Results
	      //val solrResultsArray = result._2._2.slice(0,20)
	      
	      writer.write("KBP SLOT NAME: " + kbpSlotName + "\n")
	      
	      for(slotQuery <- slotCandidateSet.candidateSets){
	      
		      val kbpOpenIEData = slotQuery.pattern
		      val candidateExtractionsList = slotQuery.candidateExtractions.slice(0,20)
		      
		      writer.write("\tQuery Data:\t"+ "RelationTerms: "+kbpOpenIEData.openIERelationString.getOrElse({""})
		          + "\t Arg2Begins: " + kbpOpenIEData.arg2Begins.getOrElse({""}) + "\t Entity In: " +
		          kbpOpenIEData.entityIn.getOrElse({""}) + "\t SlotFill In: " + kbpOpenIEData.slotFillIn.getOrElse({""}) +
		          "\t Slot type: " + kbpOpenIEData.slotType.getOrElse({""}) +"\n")
		      
		      writer.write("\tResults:\n")
		      if (candidateExtractionsList.length ==0){
		        writer.write("\t\tNil" + "\n")
		      }
		      
		      
		      for (candidateExtraction <- candidateExtractionsList){
		        
		        writer.write("\t\targ1: " + candidateExtraction.kbpExtraction.arg1.originalText + "\t rel: " + candidateExtraction.kbpExtraction.rel.originalText + 
		            "\t arg2: " + candidateExtraction.kbpExtraction.arg2.originalText + "\t docID: " + candidateExtraction.kbpExtraction.sentence.docId +
		            "\t confidence: " + candidateExtraction.kbpExtraction.confidence + "\t sentence: " + candidateExtraction.kbpExtraction.sentence.dgraph.text + "\n\n")
		
		
		      }
	      }
      }
      
      else{
         writer.write("KBP SLOT NAME: " + kbpSlot + "\n" + "\t\tNil\n")
        
      }
      
    }
      
    writer.close()
  }
  
  /*
   * Overloaded to return a string for server usage
   */
  def printUnformattedOutput(mapOfResults: Map[String,SlotCandidateSet], kbpQueryEntityType: KBPQueryEntityType):String = {
    
    val sb = new StringBuilder()
    for(kbpSlot <- SlotTypes.getSlotTypesList(kbpQueryEntityType)){
      
      if(mapOfResults.contains(kbpSlot)){
	      val kbpSlotName = kbpSlot
	      val slotCandidateSet = mapOfResults(kbpSlot)
	      
	      //only return up to 20 solr Results
	      //val solrResultsArray = result._2._2.slice(0,20)
	      
	      sb.append("KBP SLOT NAME: " + kbpSlotName + "\n")
	      
	      for(slotQuery <- slotCandidateSet.candidateSets){
	      
		      val kbpOpenIEData = slotQuery.pattern
		      val candidateExtractionsList = slotQuery.candidateExtractions.slice(0,20)
		      
		       sb.append("\tQuery Data:\t"+ "RelationTerms: "+kbpOpenIEData.openIERelationString.getOrElse({""})
		          + "\t Arg2Begins: " + kbpOpenIEData.arg2Begins.getOrElse({""}) + "\t Entity In: " +
		          kbpOpenIEData.entityIn.getOrElse({""}) + "\t SlotFill In: " + kbpOpenIEData.slotFillIn.getOrElse({""}) +
		          "\t Slot type: " + kbpOpenIEData.slotType.getOrElse({""}) +"\n")
		      
		       sb.append("\tResults:\n")
		      if (candidateExtractionsList.length ==0){
		         sb.append("\t\tNil" + "\n")
		      }
		      
		      
		      for (candidateExtraction <- candidateExtractionsList){
		        
		         sb.append("\t\targ1: " + candidateExtraction.kbpExtraction.arg1.originalText + "\t rel: " + candidateExtraction.kbpExtraction.rel.originalText + 
		            "\t arg2: " + candidateExtraction.kbpExtraction.arg2.originalText + "\t docID: " + candidateExtraction.kbpExtraction.sentence.docId +
		            "\t confidence: " + candidateExtraction.kbpExtraction.confidence + "\t sentence: " + candidateExtraction.kbpExtraction.sentence.dgraph.text + "\n\n")
		
		
		      }
	      }
      }
      
      else{
         sb.append("KBP SLOT NAME: " + kbpSlot + "\n" + "\t\tNil\n")
        
      }
      
    }
      
    sb.toString()
  }
  
  
  
  def printFormattedOutput(mapOfResults: Map[String,SlotCandidateSet], filePath: String, kbpQueryEntityType: KBPQueryEntityType){
    
    val writer = new PrintWriter(new File(filePath))
    
    //iterate over every slot type
    for(kbpSlot <- SlotTypes.getSlotTypesList(kbpQueryEntityType)){
      
      //if the kbp slot is contained in the results
      if(mapOfResults.contains(kbpSlot)){
        
          //for each slot print one response for single-valued slot
    	  //print k-slots for multi-valued slot
    	  //or print NIL
        
    	  
	      val kbpSlotName = kbpSlot
	      val slotCandidateSet = mapOfResults(kbpSlot)
	      

	      
	      val bestAnswerExtractions = slotCandidateSet.rankedAnswers
	      
	      if(!bestAnswerExtractions.isEmpty){
	        val bestAnswerExtraction = bestAnswerExtractions.head
	        val queryData = bestAnswerExtraction.pattern
        	    val slotFiller = {
        		  if(queryData.slotFillIn.get.toLowerCase().trim() == "arg1"){
        		    bestAnswerExtraction.kbpExtraction.arg1.originalText
        		  }
        		  else if(queryData.slotFillIn.get.toLowerCase().trim() == "arg2"){
        		    bestAnswerExtraction.kbpExtraction.arg2.originalText
        		  }
        		}
        		
        		val fillerOffset = {
        		  if(queryData.slotFillIn.get.toLowerCase().trim() == "arg1"){
        		    bestAnswerExtraction.kbpExtraction.arg1.tokenInterval
        		  }
        		  else if(queryData.slotFillIn.get.toLowerCase().trim() == "arg2"){
        		    bestAnswerExtraction.kbpExtraction.arg2.tokenInterval
        		  }
        		}
        		
        		val entityOffset = {
        		  if(queryData.entityIn.get.toLowerCase().trim() == "arg1"){
        		    bestAnswerExtraction.kbpExtraction.arg1.tokenInterval
        		  }
        		  else if(queryData.entityIn.get.toLowerCase().trim() == "arg2"){
        		    bestAnswerExtraction.kbpExtraction.arg2.tokenInterval
        		  }
        		} 

        		writer.write(Iterator("queryID",kbpSlot,"runID",bestAnswerExtraction.kbpExtraction.sentence.docId,slotFiller,
        		    fillerOffset,entityOffset,bestAnswerExtraction.kbpExtraction.rel.tokenInterval,
        		    bestAnswerExtraction.kbpExtraction.confidence).mkString("\t") + "\n")
	        
	        
	      }
	      else{
	        writer.write(Iterator("queryID",kbpSlot,"runID","NIL").mkString("\t") + "\n")
	      }
      }
      
      else{
        // else if the results Map does not contain the slot
        // print nothing since this slot is ignored
        
      }
      
    }
      
    writer.close()
  }
  
  
  
  
  /**
   * Overloaded to return a string for server usage
   */
  def printFormattedOutput(mapOfResults: Map[String,SlotCandidateSet], kbpQueryEntityType: KBPQueryEntityType): String = {
    
    val sb = new StringBuilder()
    
    //iterate over every slot type
    for(kbpSlot <- SlotTypes.getSlotTypesList(kbpQueryEntityType)){
      
      //if the kbp slot is contained in the results
      if(mapOfResults.contains(kbpSlot)){
        
          //for each slot print one response for single-valued slot
    	  //print k-slots for multi-valued slot
    	  //or print NIL
        
    	  
	      val kbpSlotName = kbpSlot
	      val slotCandidateSet = mapOfResults(kbpSlot)
	      

	      
	      val bestAnswerExtractions = slotCandidateSet.rankedAnswers
	      
	      if(!bestAnswerExtractions.isEmpty){
	        val bestAnswerExtraction = bestAnswerExtractions.head
	        val queryData = bestAnswerExtraction.pattern
        	    val slotFiller = {
        		  if(queryData.slotFillIn.get.toLowerCase().trim() == "arg1"){
        		    bestAnswerExtraction.kbpExtraction.arg1.originalText
        		  }
        		  else if(queryData.slotFillIn.get.toLowerCase().trim() == "arg2"){
        		    bestAnswerExtraction.kbpExtraction.arg2.originalText
        		  }
        		}
        		
        		val fillerOffset = {
        		  if(queryData.slotFillIn.get.toLowerCase().trim() == "arg1"){
        		    bestAnswerExtraction.kbpExtraction.arg1.tokenInterval
        		  }
        		  else if(queryData.slotFillIn.get.toLowerCase().trim() == "arg2"){
        		    bestAnswerExtraction.kbpExtraction.arg2.tokenInterval
        		  }
        		}
        		
        		val entityOffset = {
        		  if(queryData.entityIn.get.toLowerCase().trim() == "arg1"){
        		    bestAnswerExtraction.kbpExtraction.arg1.tokenInterval
        		  }
        		  else if(queryData.entityIn.get.toLowerCase().trim() == "arg2"){
        		    bestAnswerExtraction.kbpExtraction.arg2.tokenInterval
        		  }
        		} 

        		sb.append(Iterator("queryID",kbpSlot,"runID",bestAnswerExtraction.kbpExtraction.sentence.docId,slotFiller,
        		    fillerOffset,entityOffset,bestAnswerExtraction.kbpExtraction.rel.tokenInterval,
        		    bestAnswerExtraction.kbpExtraction.confidence).mkString("\t") + "\n")
	        
	        
	      }
	      else{
	        sb.append(Iterator("queryID",kbpSlot,"runID","NIL").mkString("\t") + "\n")
	      }
      }
      
      else{
        // else if the results Map does not contain the slot
        // print nothing since this slot is ignored
        
      }
      
    }
      
    sb.toString
  }
  
  def printFormattedOutputForKBPQuery(mapOfResults: Map[String,SlotCandidateSet], filePath: String, kbpQuery: KBPQuery){
    
    val writer = new FileWriter(new File(filePath),true)
    
    //iterate over every slot type
    for(kbpSlot <- SlotTypes.getSlotTypesList(kbpQuery.entityType)){
      
      //if the kbp slot is contained in the results
      if(mapOfResults.contains(kbpSlot)){
        
          //for each slot print one response for single-valued slot
    	  //print k-slots for multi-valued slot
    	  //or print NIL
        
    	  
	      val kbpSlotName = kbpSlot
	      val slotCandidateSet = mapOfResults(kbpSlot)
	      
	      //for now assume every slot is single valued, use a
	      //separate filter method to choose best answer
	      
	      val bestAnswerExtractions = slotCandidateSet.rankedAnswers
	      
	      if(!bestAnswerExtractions.isEmpty){
	        val bestAnswerExtraction = bestAnswerExtractions.head
	        val queryData = bestAnswerExtraction.pattern
	        		val slotFiller = {
	        		  if(queryData.slotFillIn.get.toLowerCase().trim() == "arg1"){
	        		    bestAnswerExtraction.kbpExtraction.arg1.originalText
	        		  }
	        		  else if(queryData.slotFillIn.get.toLowerCase().trim() == "arg2"){
	        		    bestAnswerExtraction.kbpExtraction.arg2.originalText
	        		  }
	        		}
	        		
	        		val fillerOffsetInterval = {
	        		  if(queryData.slotFillIn.get.toLowerCase().trim() == "arg1"){
	        		    bestAnswerExtraction.kbpExtraction.arg1.tokenInterval
	        		  }
	        		  else if(queryData.slotFillIn.get.toLowerCase().trim() == "arg2"){
	        		    bestAnswerExtraction.kbpExtraction.arg2.tokenInterval
	        		  }
	        		  else if(queryData.slotFillIn.get.toLowerCase().trim() == "relation"){
	        		    bestAnswerExtraction.kbpExtraction.rel.tokenInterval
	        		  }
	        		  else{
	        		    throw new Exception("slotFillIn specification should only be arg1, arg2, or relation")
	        		  }
	        		}
	        		
	        		val entityOffsetInterval = {
	        		  if(queryData.entityIn.get.toLowerCase().trim() == "arg1"){
	        		    bestAnswerExtraction.kbpExtraction.arg1.tokenInterval
	        		  }
	        		  else if(queryData.entityIn.get.toLowerCase().trim() == "arg2"){
	        		    bestAnswerExtraction.kbpExtraction.arg2.tokenInterval
	        		  }
	        		  else{
	        		    throw new Exception("entityIn specification should only be arg1 or arg2")
	        		  }
	        		} 
	        		
	   

	        		val startOffset = bestAnswerExtraction.kbpExtraction.sentence.startOffset
	        		
	        		val fillerOffsetStart = startOffset + fillerOffsetInterval.start
	        		val fillerOffsetEnd = startOffset + fillerOffsetInterval.end
	        		val fillerOffsetString = fillerOffsetStart.toString + "-" + fillerOffsetEnd.toString()
	        		
	        		val entityOffsetStart = startOffset + entityOffsetInterval.start
	        		val entityOffsetEnd = startOffset + entityOffsetInterval.end
	        		val entityOffsetString = entityOffsetStart.toString + "-" + entityOffsetEnd.toString()
	        		
	        		val justificationOffsetStart = startOffset + bestAnswerExtraction.kbpExtraction.arg1.tokenInterval.start
	        		val justificationOffsetEnd = startOffset + bestAnswerExtraction.kbpExtraction.arg2.tokenInterval.end
	        		val justificationOffsetString = justificationOffsetStart.toString() +"-" + justificationOffsetEnd.toString()
	        		
	        		
	        		writer.write(Iterator(kbpQuery.id,kbpSlot,runID,bestAnswerExtraction.kbpExtraction.sentence.docId,slotFiller,
	        		    fillerOffsetString,entityOffsetString,justificationOffsetString,bestAnswerExtraction.kbpExtraction.confidence).mkString("\t") + "\n")
	        
	        
	      }
	      else{
	        //no answer found
	        writer.write(Iterator(kbpQuery.id,kbpSlot,runID,"NIL").mkString("\t") + "\n")
	      }
	      
	      
      }
      
      else{
        // else if the results Map does not contain the slot
        // print nothing since this slot is ignored
        
      }
      
    }
      
    writer.close()
  }
  


}