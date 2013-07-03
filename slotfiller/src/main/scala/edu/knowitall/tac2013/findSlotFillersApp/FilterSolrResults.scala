package edu.knowitall.tac2013.findSlotFillersApp

import edu.knowitall.tac2013.openie.KbpExtraction

object FilterSolrResults {

  //filter for arg2 beginning with proper preposition
  private def satisfiesArg2PrepositionFilter(relationData: KbpSlotToOpenIEData, candidateExtraction: CandidateExtraction ): Boolean ={
    
      val kbpExtraction = candidateExtraction.kbpExtraction
      if (relationData.arg2Begins.nonEmpty){
	      val solrResultsArg2 = kbpExtraction.arg2.originalText
	      val arg2PrepositionString = relationData.arg2Begins.get.trim()
	      
	      if(solrResultsArg2.toString().toLowerCase().substring(0,arg2PrepositionString.length) == arg2PrepositionString.toLowerCase())
	        return true
	      
      	  else{
            return false
          }
      }
      else{
        return true
      }
  }
  
  private def satisfiesRelFilter(relationData: KbpSlotToOpenIEData, candidateExtraction: CandidateExtraction ): Boolean ={
    
    val kbpExtraction = candidateExtraction.kbpExtraction
    if(relationData.isValid){
      
      if(!relationData.openIERelationString.get.contains("<JobTitle>")){
		    
		    val relationTerms = relationData.openIERelationString.get.trim().split(" ")
		    
		    val relationTermsReversed =  relationTerms.reverse
		    
		    val relationTermsFromExtraction = kbpExtraction.rel.originalText.trim().split(" ")
		    
		    
		    var count = 0 
		    for(term <- relationTermsReversed){
		      val sentenceWord = relationTermsFromExtraction(relationTermsFromExtraction.length-1-count)
		      if ((term.toLowerCase() != sentenceWord.toLowerCase()) &&
		          (term.replace("[", "").replace("]", "").toLowerCase() != sentenceWord.replace("[", "").replace("]", "").toLowerCase())){
		         return false
		      }
		      count = count +1
		    }
		    return true
	    }
      else{
        val chunkedSentence = kbpExtraction.sentence.chunkedTokens
        val types = SemanticTaggers.useJobTitleTagger(chunkedSentence)
        val relLocation = kbpExtraction.rel.tokenInterval
        
        for(t <- types){
          if(t.interval().intersects(relLocation)){
            return true
          }
        }
        return false
      }
    }
    
    else{
      
       throw new Exception("KbpSlotToOpenIEData instance is not valid.")
       false
    }
  }
  
  private def satisfiesEntityFilter(relationData: KbpSlotToOpenIEData, candidateExtraction: CandidateExtraction, queryEntity: String ): Boolean ={
    
    val kbpExtraction = candidateExtraction.kbpExtraction
    val candidateType = candidateExtraction.candidateType
    if(candidateType == CandidateType.REGULAR){
	    if(relationData.isValid){
	      
	        val entityIn = relationData.entityIn.get.trim()
	        val entityFromExtraction = entityIn.toLowerCase() match{
	          case "arg1" => kbpExtraction.arg1.originalText
	          case "arg2" => kbpExtraction.arg2.originalText
	          case _ => throw new Exception("Poorly formatted entityIn field, should be arg1 or arg2")
	        }
	
		    
		    val entityFromExtractionSplit = entityFromExtraction.toString().split(" ")
		    
		    val queryEntityReversedSplit = queryEntity.trim().split(" ").reverse
		    
		    var count = 0
		    for(term <- queryEntityReversedSplit){
		      val extractionWord = entityFromExtractionSplit(entityFromExtractionSplit.length-1-count)
		      if(term.toLowerCase() != extractionWord.toLowerCase()){
		          //(term.toLowerCase() != extractionWord.substring(1,extractionWord.length-1).toLowerCase())){
		         return false
		      }
		      
		      count = count + 1
		    }
		    
		    true
	    }
	    else{
	    
	      throw new Exception("KbpSlotToOpenIEData instance is not valid.")
	      false
	    }
    }
    else{
      true
    }
  }
  
  private def satisfiesSemanticFilter(relationData: KbpSlotToOpenIEData, candidateExtraction: CandidateExtraction) : Boolean = {
    
    val slotType = relationData.slotType.getOrElse({""})
    val kbpExtraction = candidateExtraction.kbpExtraction
    val slotLocation = relationData.slotFillIn match {
      case Some("arg1") => kbpExtraction.arg1.tokenInterval
      case Some("arg2") => kbpExtraction.arg2.tokenInterval
      case Some("relation") => kbpExtraction.rel.tokenInterval
      case _ => return false
    }
   
    val chunkedSentence = kbpExtraction.sentence.chunkedTokens

    
    if(slotType == "Organization" || slotType =="Person" || slotType =="Stateorprovince" ||
        slotType == "City" || slotType == "Country"){
	    val types = SemanticTaggers.useStandfordNERTagger(chunkedSentence)
	    
	    
	    
	    for (t <- types){
	      if (t.interval().intersects(slotLocation)){
		      slotType match {
		        case "Organization" => {
		          if (t.descriptor() == "StanfordORGANIZATION"){
		            return true
		          }
		        }
		        case "Person" => {
		          if (t.descriptor() == "StanfordPERSON"){
		            return true
		          }
		        }
		        // default case will be location
		        case _ => {
		          if (t.descriptor() == "StanfordLOCATION"){
		            return true
		          }
		        }
		      }
	      }
	    }
	    
	    return false
    }
    
    else if(slotType == "School"){
      
        val types = SemanticTaggers.useEducationalOrganizationTagger(chunkedSentence)
        
        for(t <- types){
          if (t.interval().intersects(slotLocation)) return true
          
        }
        
        return false
      
    }
    
    else if(slotType == "JobTitle"){
      
        val types = SemanticTaggers.useJobTitleTagger(chunkedSentence)
        
        for(t <- types){
          if (t.interval().intersects(slotLocation)) return true
          
        }
        
        return false
      
    }
    
    else if(slotType == "Nationality"){
      
        val types = SemanticTaggers.useNationalityTagger(chunkedSentence)
        
        for(t <- types){
          if (t.interval().intersects(slotLocation)) return true
          
        }
        
        return false
      
    }
    
    else if(slotType == "Religion"){
      
        val types = SemanticTaggers.useReligionTagger(chunkedSentence)
        
        for(t <- types){
          if (t.interval().intersects(slotLocation)) return true
          
        }
        
        return false
      
    }
    else if (slotType == "Date"){
        val types = SemanticTaggers.useDateTagger(chunkedSentence)
        
        for(t <- types){
          if (t.interval().intersects(slotLocation)) return true
          
        }
        
        return false
      
    }
    else{
      
        return true
      
    }
    
    
  }
  
  //filters results from solr by calling helper methods that look at the KbpSlotToOpenIEData specifications and compare
  //that data with the results from solr to see if the relation is still a candidate
  //
  def filterResults(resultsList: List[CandidateExtraction],relationData: KbpSlotToOpenIEData, queryEntity: String) : List[CandidateExtraction] = {
    
    var filteredResultsList = List[CandidateExtraction]()
    //loop over each solr result
    for(candidateExtraction <- resultsList){
      
      
      
      if( satisfiesArg2PrepositionFilter(relationData,candidateExtraction) &&
          satisfiesEntityFilter(relationData,candidateExtraction,queryEntity) &&
          satisfiesRelFilter(relationData,candidateExtraction) &&
          satisfiesSemanticFilter(relationData,candidateExtraction)){
        
          filteredResultsList = filteredResultsList ::: List(candidateExtraction)
          
      }
      
      
       
    }
    
    //sort array by confidence
    
    filteredResultsList
  }
}