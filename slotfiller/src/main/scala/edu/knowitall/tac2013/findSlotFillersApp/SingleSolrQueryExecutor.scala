package edu.knowitall.tac2013.findSlotFillersApp
import scala.util.parsing.json.JSONObject
import jp.sf.amateras.solr.scala._
import edu.knowitall.tac2013.openie.KbpExtraction
import edu.knowitall.tac2013.openie.solr.KbpExtractionConverter




object SingleSolrQueryExecutor {
  import jp.sf.amateras.solr.scala._
  lazy val client = new SolrClient("http://knowitall:knowit!@rv-n16.cs.washington.edu:9321/solr")

  
  def issueSolrQuery(queryString: String, candidateType: CandidateType.Value, pattern: KbpSlotToOpenIEData): List[CandidateExtraction] = {
    //not sure where the best place to put this val is so I'm hoping making it lazy
    //will be a good idea
    
    val query = client.query(queryString)
    val result = query.sortBy("confidence",Order.desc).rows(10000).getResultAsMap()
    

    val extrs = result.documents.flatMap { doc =>
      val fieldMap = doc.asInstanceOf[Map[String, Any]]
      KbpExtraction.fromFieldMap(fieldMap) match{
        case Some(x) => { Option(new CandidateExtraction(x,candidateType,pattern))}
        case None => { None}
      }
    }
    extrs
  }
}
