package edu.knowitall.tac2013.openie.solr

import scopt.OptionParser
import jp.sf.amateras.solr.scala.SolrClient
import edu.knowitall.tac2013.openie.KbpExtraction

/**
 * A simple class for querying solr and dumping results to stdout.
 */
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
    
    val extrs = result.documents.flatMap { doc =>
      val fieldMap = doc.asInstanceOf[Map[String, String]]
      KbpExtraction.fromFieldMap(fieldMap)
    } 
    
    System.err.println("%d exrs retrieved.".format(extrs.size))
    
    extrs foreach { extr =>
      val serialized = KbpExtraction.write(extr)
      println(serialized)
    }
  }
}