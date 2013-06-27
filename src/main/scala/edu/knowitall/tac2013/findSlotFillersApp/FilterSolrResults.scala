package edu.knowitall.tac2013.findSlotFillersApp

import edu.knowitall.tac2013.openie.KbpExtraction

object FilterSolrResults {

  //filter for arg2 beginning with proper preposition
  private def satisfiesArg2PrepositionFilter(relationData: KbpSlotToOpenIEData, kbpExtraction: KbpExtraction ): Boolean ={
      if (relationData.arg2Begins.nonEmpty){
	      val solrResultsArg2 = kbpExtraction.arg2.originalText
	      val arg2PrepositionString = relationData.arg2Begins.get.trim()
	      
	      if(solrResultsArg2.toString().toLowerCase().substring(0,arg2PrepositionString.length) == arg2PrepositionString.toLowerCase() ||
	          solrResultsArg2.toString().toLowerCase().substring(1,arg2PrepositionString.length+1) == arg2PrepositionString.toLowerCase())
	        true
	      
      	  else{
            false
          }
      }
      else{
        true
      }
  }
  
  private def satisfiesRelFilter(relationData: KbpSlotToOpenIEData, kbpExtraction: KbpExtraction ): Boolean ={
    
    if(relationData.isValid){
	    
	    val relationTerms = relationData.openIERelationString.get.trim().split(" ")
	    
	    val relationTermsReversed =  relationTerms.reverse
	    
	    val relationTermsFromExtraction = kbpExtraction.rel.originalText.trim().split(" ")
	    
	    
	    var count = 0 
	    for(term <- relationTermsReversed){
	      val sentenceWord = relationTermsFromExtraction(relationTermsFromExtraction.length-1-count)
	      if( (term.toLowerCase() != sentenceWord.toLowerCase()) &&
	          (term.toLowerCase() != sentenceWord.substring(1,sentenceWord.length-1).toLowerCase())){
	         return false
	      }
	      count = count +1
	    }
	    true
    }
    
    else{
      
       throw new Exception("KbpSlotToOpenIEData instance is not valid.")
       false
    }
  }
  
  private def satisfiesEntityFilter(relationData: KbpSlotToOpenIEData, kbpExtraction: KbpExtraction, queryEntity: String ): Boolean ={
    
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
	      if((term.toLowerCase() != extractionWord.toLowerCase()) &&
	          (term.toLowerCase() != extractionWord.substring(1,extractionWord.length-1).toLowerCase())){
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
  
  //filters results from solr by calling helper methods that look at the KbpSlotToOpenIEData specifications and compare
  //that data with the results from solr to see if the relation is still a candidate
  //
  def filterResults(resultsList: List[KbpExtraction],relationData: KbpSlotToOpenIEData, queryEntity: String) : List[KbpExtraction] = {
    
    var filteredResultsList = List[KbpExtraction]()
    //loop over each solr result
    for(kbpExtraction <- resultsList){
      
      
      
      if( satisfiesArg2PrepositionFilter(relationData,kbpExtraction) &&
          satisfiesEntityFilter(relationData,kbpExtraction,queryEntity) &&
          satisfiesRelFilter(relationData,kbpExtraction)){
        
          filteredResultsList = filteredResultsList ::: List(kbpExtraction)
          
      }
      
      
       
    }
    
    //sort array by confidence
    
    filteredResultsList
  }
}