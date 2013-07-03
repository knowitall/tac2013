package edu.knowitall.tac2013.findSlotFillersApp

import jp.sf.amateras.solr.scala._
import SingleSolrQueryExecutor.issueSolrQuery
import FilterSolrResults.filterResults
import edu.knowitall.tac2013.openie.KbpExtraction



object QueryEntityForAllSlots {
  
  //takes entity string and map from KBP slot strings to Open IE relation strings and runs queries
  //to our solr instance for every type of OpenIERelation
  def executeEntityQueryForAllSlots(queryEntity: String, KBPOpenIERelationMap: Map[String,List[KbpSlotToOpenIEData]], nodeID: String = ""):
    Map[String,List[CandidateSet]] ={
    
    //var resultsList = List[(String,KbpSlotToOpenIEData,List[KbpExtraction])]()
    var resultsMap = Map[String,List[CandidateSet]]()
    //for every relevant slot 
    for( pair <- KBPOpenIERelationMap){
      
      var resultsList = List[CandidateSet]()
      //for every different query formulation
      for(relationData <- pair._2){
        
        //make a query for each instance of relation data specifying where to search for the entity
        //string and what prepositions should be included in arg2, among other things
        
        //first check if the specifications in the KBPSlotToOpenIEData are valid
        if(relationData.isValid()){
	        val qb = new QueryBuilder //solr query builder
	        qb.buildQuery(relationData,queryEntity)
	        val queryString = qb.getQueryString
	        
	        println(queryString)
	        
	        //issue query (don't cut off results yet)
	        val listOfResults = issueSolrQuery(queryString,CandidateType.REGULAR,relationData)
	        var combinedListOfResults : Option[List[CandidateExtraction]] = None
	        
	        //issue an entity link query too if it was given
	        if(nodeID != ""){
		        val qbLink = new QueryBuilder
		        qbLink.buildLinkedQuery(relationData,nodeID)
		        val linkQueryString = qbLink.getQueryString
		        println(linkQueryString)
		        combinedListOfResults = Some(listOfResults ::: issueSolrQuery(linkQueryString,CandidateType.LINKED,relationData))
	        }
	        
	        //filter
	        val listOfFilteredResults = filterResults(combinedListOfResults.getOrElse(listOfResults),relationData,queryEntity)
	        
	        
	        //construct tuple entry to go into results Array
	        val cs = new CandidateSet(relationData,listOfFilteredResults)
	        resultsList = resultsList ::: List(cs)
        }
      }
      
      //store list of query formulations and solr results with the string
      //of the slot
      
      resultsMap += (pair._1 -> resultsList)
      
    }
    resultsMap
  }
  
  
  //takes entity string and map from KBP slot strings to Open IE relation strings and runs queries
  //to our solr instance for every type of OpenIERelation, this method uses no filters, this is for debugging purposes
  def executeEntityQueryForAllSlotsWithoutFilter(queryEntity: String, KBPOpenIERelationMap: Map[String,List[KbpSlotToOpenIEData]], nodeID: String = ""):
    Map[String,List[CandidateSet]] ={
    
    //var resultsList = List[(String,KbpSlotToOpenIEData,List[KbpExtraction])]()
    var resultsMap = Map[String,List[CandidateSet]]()
    //for every relevant slot 
    for( pair <- KBPOpenIERelationMap){
      
      var resultsList = List[CandidateSet]()
      //for every different query formulation
      for(relationData <- pair._2){
        
        //make a query for each instance of relation data specifying where to search for the entity
        //string and what prepositions should be included in arg2, among other things
        
        //first check if the specifications in the KBPSlotToOpenIEData are valid
        if(relationData.isValid()){
	        val qb = new QueryBuilder //solr query builder
	        qb.buildQuery(relationData,queryEntity)
	        val queryString = qb.getQueryString
	        
	        println(queryString)
	        
	        //issue query (don't cut off results yet)
	        val listOfResults = issueSolrQuery(queryString,CandidateType.REGULAR,relationData)
	        var combinedListOfResults : Option[List[CandidateExtraction]] = None
	        
	        //issue an entity link query too if it was given
	        if(nodeID != ""){
		        val qbLink = new QueryBuilder
		        qbLink.buildLinkedQuery(relationData,nodeID)
		        val linkQueryString = qbLink.getQueryString
		        println(linkQueryString)
		        combinedListOfResults = Some(listOfResults ::: issueSolrQuery(linkQueryString,CandidateType.LINKED,relationData))
	        }
	        
	        //filter
	        //val listOfFilteredResults = filterResults(combinedListOfResults.getOrElse(listOfResults),relationData,queryEntity)
	        
	        
	        //construct tuple entry to go into results Array
	        val cs = new CandidateSet(relationData,combinedListOfResults.getOrElse(listOfResults))
	        resultsList = resultsList ::: List(cs)
        }
      }
      
      //store list of query formulations and solr results with the string
      //of the slot
      
      resultsMap += (pair._1 -> resultsList)
      
    }
    resultsMap
  }
}