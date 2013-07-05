package edu.knowitall.tac2013.solr.populate

import scopt.OptionParser
import jp.sf.amateras.solr.scala.SolrClient
import edu.knowitall.tac2013.openie.KbpExtraction
import jp.sf.amateras.solr.scala.Order
import scala.Option.option2Iterable

/**
 * A simple class for querying solr and dumping results to stdout.
 */
class SolrSimpleExecutor private (val client: SolrClient, val maxResults: Int) {
  
  def this(solrUrl: String, maxResults: Int) = this(new SolrClient(solrUrl), maxResults)
  
  def query(queryString: String): Response = {
    
    val query = client.query(queryString)
    val result = query.sortBy("confidence", Order.desc).rows(maxResults).getResultAsMap()
    val numResults = result.numFound
    Response(numResults, result.documents.flatMap { fieldMap =>
      KbpExtraction.fromFieldMap(fieldMap)
    })
  }
}

case class Response(numResults: Long, results: Seq[KbpExtraction])

object SolrSimpleExecutor {

  
  def main(args: Array[String]): Unit = {
    
    var solrUrl = ""
    var queryString = ""
      
    val parser = new OptionParser() {
      arg("solrUrl", "Solr URL.", { solrUrl = _ })
      arg("query", "Solr Query String.", { queryString = _})
    }  
    
    if (!parser.parse(args)) return
    
    val client = new SolrClient(solrUrl)
    
    val query = client.query(queryString)
    
    val result = query.rows(1000).getResultAsMap()
    
    System.err.println("%d results retrieved.".format(result.numFound))
    
    val extrs = result.documents.flatMap { fieldMap =>
      KbpExtraction.fromFieldMap(fieldMap)
    } 
    
    System.err.println("%d exrs retrieved.".format(extrs.size))
    
    extrs foreach { extr =>
      val serialized = KbpExtraction.write(extr)
      println(serialized)
    }
  }
}