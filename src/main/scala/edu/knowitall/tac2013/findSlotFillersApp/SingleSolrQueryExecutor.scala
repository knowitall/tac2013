package edu.knowitall.tac2013.findSlotFillersApp
import scala.util.parsing.json.JSONObject
import jp.sf.amateras.solr.scala._

class SingleSolrQueryExecutor {

}

object SingleSolrQueryExecutor {
  import jp.sf.amateras.solr.scala._
  def getJSONResultsArray(solrQueryString: String): Array[JSONObject] = {
    var JSONResults = Array[JSONObject]()
    val client = new SolrClient("http://knowitall:knowit!@rv-n16.cs.washington.edu:8987/solr")
  
    //val q = client.query("+rel:\"born in\"")
    //val result = q.fields("arg1", "rel", "arg2").rows(10).getResultAsMap()
    val q = client.query(solrQueryString)
    val result = q.rows(100).getResultAsMap()
  
    //var jsonList = List()s
    
    result.documents.foreach { doc: Map[String,Any] =>
     //println(doc("arg1") + " " + doc("rel") + " " + doc("arg2"));
     val j  = new JSONObject(doc);
     //JSONResults.update(JSONResults.length, j)
     //JSONResults.apply(i)
     JSONResults = JSONResults :+ j
     //jsonList = (new JSONObject(doc)) :: jsonList
     }
     JSONResults
  }
  
  def issueSolrQuery(queryString: String): Array[Map[String,Any]] = {
    //not sure where the best place to put this val is so I'm hoping making it lazy
    //will be a good idea
    lazy val client = new SolrClient("http://knowitall:knowit!@rv-n16.cs.washington.edu:8995/solr")
    
    val query = client.query(queryString)
    val result = query.rows(5).getResultAsMap()
    result.documents.foreach {doc: Map[String,Any] =>
        println(doc("arg1") + " " + doc("rel") + " " + doc("arg2"))}
    val bigresult = query.rows(1000).sortBy("confidence",Order.desc).getResultAsMap()
    var a = Array[Map[String,Any]]()
    bigresult.documents.foreach {doc: Map[String,Any] =>
        a = a :+ doc
      }
    a
  }
}
