package edu.knowitall.tac2013.findSlotFillersApp

object FilterSolrResults {

  //filter for arg2 beginning with proper preposition
  private def satisfiesArg2PrepositionFilter(relationData: KbpSlotToOpenIEData, resultsMap: Map[String,Any] ): Boolean ={
      if (relationData.arg2Begins.nonEmpty){
	      val solrResultsArg2 = resultsMap("arg2")
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
  
  private def satisfiesRelFilter(relationData: KbpSlotToOpenIEData, resultsMap: Map[String,Any] ): Boolean ={
    
    if(relationData.isValid){
	    
	    val relationTerms = relationData.openIERelationString.get.trim().split(" ")
	    
	    val relationTermsReversed =  relationTerms.reverse
	    
	    val relationTermsFromExtraction = resultsMap("rel").toString.trim().split(" ")
	    
//	    println("Filtering:")
//	    println("Relation In Exctraction: " + resultsMap("rel"))
//	    println("Relation should end with: " + relationTermsReversed)
	    
	    var count = 0 
	    for(term <- relationTermsReversed){
	      val sentenceWord = relationTermsFromExtraction(relationTermsFromExtraction.length-1-count)
	     // println("Comparison: Term = " + term + " sentenceWord = " + sentenceWord)
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
  
  private def satisfiesEntityFilter(relationData: KbpSlotToOpenIEData, resultsMap: Map[String,Any], queryEntity: String ): Boolean ={
    
    if(relationData.isValid){
	    val entityFromExtraction = resultsMap(relationData.entityIn.get.trim())
	    val parseFromExtraction = resultsMap(relationData.entityIn.get+"_postag")
	    
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
  def filterResults(resultsArray: Array[Map[String,Any]],relationData: KbpSlotToOpenIEData, queryEntity: String) : Array[Map[String,Any]] = {
    
    var filteredResultsArray = Array[Map[String,Any]]()
    //loop over each solr result
    for(solrResultsMap <- resultsArray){
      
      
      
      if( satisfiesArg2PrepositionFilter(relationData,solrResultsMap) &&
          satisfiesEntityFilter(relationData,solrResultsMap,queryEntity) &&
          satisfiesRelFilter(relationData,solrResultsMap)){
        
          filteredResultsArray = filteredResultsArray :+ solrResultsMap
          
      }
      
      
       
    }
    
    //sort array by confidence
    
    filteredResultsArray
  }
}