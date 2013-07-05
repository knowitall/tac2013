package edu.knowitall.tac2013.solr.query

import jp.sf.amateras.solr.scala._
import edu.knowitall.tac2013.findSlotFillersApp.FilterSolrResults.filterResults
import edu.knowitall.tac2013.openie.KbpExtraction
import edu.knowitall.tac2013.findSlotFillersApp.Candidate
import edu.knowitall.tac2013.findSlotFillersApp.KBPQuery
import edu.knowitall.tac2013.findSlotFillersApp.SlotPattern
import edu.knowitall.taggers.Type
import scala.Option.option2Iterable
import edu.knowitall.tac2013.findSlotFillersApp.SemanticTaggers.getTagTypes

class SolrQueryExecutor(val solrClient: SolrClient) {
  
  def this(url: String) = this(new SolrClient(url))
  
  private def issueSolrQuery(kbpSolrQuery: SolrQuery): List[Candidate] = {
    
    println(kbpSolrQuery.queryString)
    
    // issue query
    val query = solrClient.query(kbpSolrQuery.queryString)
    val result = query.sortBy("confidence",Order.desc).rows(10000).getResultAsMap()

    // load as KbpExtraction
    val kbpExtrs = result.documents.flatMap { doc =>
      val fieldMap = doc.asInstanceOf[Map[String, Any]]
      KbpExtraction.fromFieldMap(fieldMap)
      
    }
    
    // wrap with Candidate
    kbpExtrs.map { extr =>
      new Candidate(kbpSolrQuery.pattern, kbpSolrQuery.resultType, extr, 
          getTagTypes(extr,kbpSolrQuery.pattern))
    }
  }
  
  //takes entity string and map from KBP slot strings to Open IE relation strings and runs queries
  //to our solr instance for every type of OpenIERelation
  def executeQuery(kbpQuery: KBPQuery, slot: String): Seq[Candidate] = {

    val unfilteredCandidates = executeUnfilteredQuery(kbpQuery, slot)

    val filteredCandidateSets = filterResults(unfilteredCandidates, kbpQuery.name)

    filteredCandidateSets
  }

  //takes entity string and map from KBP slot strings to Open IE relation strings and runs queries
  //to our solr instance for every type of OpenIERelation, this method uses no filters, this is for debugging purposes
  def executeUnfilteredQuery(kbpQuery: KBPQuery, slot: String): Seq[Candidate] = {

    val patterns = SlotPattern.patternsForQuery(kbpQuery)(slot)

    val solrQueries = patterns.flatMap { pattern => 
      val queryBuilder = new SolrQueryBuilder(pattern, kbpQuery)
      queryBuilder.getQueries 
    }
    
    val allResults = solrQueries.flatMap { q => issueSolrQuery(q) }
    
    allResults
  }
}

object SolrQueryExecutor {
  
  val defaultSolrUrl = "http://knowitall:knowit!@rv-n16.cs.washington.edu:9321/solr"
    
  lazy val defaultInstance = new SolrQueryExecutor(defaultSolrUrl) 
}