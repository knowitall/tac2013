package edu.knowitall.tac2013.solr.query

object SolrHelper {
  
  def getDocIDMapToSentNumsForEntityNameAndNodeID(entityName: String, nodeID: Option[String]) : Map[String,List[(String,Int)]] ={
 
    val client  = SolrQueryExecutor.defaultInstance.solrClient
    
    var solrEntityQueryString = "arg1Text:" + "\"" + entityName + "\" " + "arg2Text:" + "\"" + entityName + "\" "
    if(nodeID.isDefined){
      solrEntityQueryString += "arg1WikiLinkNodeId:" + "\"" + nodeID.get + "\" " + "arg2WikiLinkNodeId:" + "\"" + nodeID.get + "\" "
    }
    
    val docIDQuery = client.query(solrEntityQueryString)
    val result = docIDQuery.rows(1000000).getResultAsMap()
    val docSentNumPairs = for(r <- result.documents) yield {
      (r("docId").toString,r("sentNum").toString().toInt)
    }
    val docIdMapListOfSentNums = docSentNumPairs.groupBy(x => x._1)
    docIdMapListOfSentNums
    
  }

}