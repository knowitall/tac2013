package edu.knowitall.tac2013.findSlotFillersApp
import scala.util.parsing.json.JSONObject
import jp.sf.amateras.solr.scala._
import edu.knowitall.tac2013.openie.KbpExtraction
import edu.knowitall.tac2013.openie.solr.KbpExtractionConverter



object SingleSolrQueryExecutor {
  import jp.sf.amateras.solr.scala._

  
  def issueSolrQuery(queryString: String): List[KbpExtraction] = {
    //not sure where the best place to put this val is so I'm hoping making it lazy
    //will be a good idea
    lazy val client = new SolrClient("http://knowitall:knowit!@rv-n16.cs.washington.edu:9321/solr")
    
    val query = client.query(queryString)
    val result = query.rows(10000).sortBy("confidence",Order.desc).getResultAsMap()
    

    val extrs = result.documents.flatMap { doc =>
      val fieldMap = doc.asInstanceOf[Map[String, Any]]
      KbpExtraction.fromFieldMap(fieldMap)
    }
    extrs
  }
}
