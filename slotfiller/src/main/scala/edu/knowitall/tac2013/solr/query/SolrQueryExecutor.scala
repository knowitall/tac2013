package edu.knowitall.tac2013.solr.query

import jp.sf.amateras.solr.scala._
import edu.knowitall.tac2013.findSlotFillersApp.FilterSolrResults.filterResults
import edu.knowitall.tac2013.openie.KbpExtraction
import edu.knowitall.tac2013.findSlotFillersApp.Candidate
import edu.knowitall.tac2013.findSlotFillersApp.Slot
import edu.knowitall.tac2013.findSlotFillersApp.KBPQuery
import edu.knowitall.tac2013.findSlotFillersApp.SlotPattern
import edu.knowitall.taggers.Type
import scala.Option.option2Iterable
import edu.knowitall.tac2013.findSlotFillersApp.SemanticTaggers.getTagTypes

class SolrQueryExecutor(val solrClient: SolrClient) {
  
  val queryCounter = new java.util.concurrent.atomic.AtomicInteger
  
  def this(url: String) = this(new SolrClient(url))
  
  private def issueSolrQuery(kbpSolrQuery: SolrQuery): Seq[Candidate] = {
    
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
      new Candidate(queryCounter.getAndIncrement, kbpSolrQuery, extr, 
          getTagTypes(extr,kbpSolrQuery.pattern))
    }
  }
  
  //takes entity string and map from KBP slot strings to Open IE relation strings and runs queries
  //to our solr instance for every type of OpenIERelation, this method uses no filters, this is for debugging purposes
  def executeUnfilteredQuery(kbpQuery: KBPQuery, slot: Slot): Seq[Candidate] = {

    val patterns = slot.patterns

    val solrQueries = patterns.flatMap { pattern => 
      val queryBuilder = new SolrQueryBuilder(pattern, kbpQuery)
      queryBuilder.getQueries 
    }
    
    val allResults = solrQueries.flatMap { q => issueSolrQuery(q) } toSeq
    
    // deduplicate identical extractions
    val deduplicated = allResults.groupBy(_.deduplicationKey).map { case (key, duplicates) =>
      // duplicates should all be same, i.e. same confidence, etc... but explicitly take highest conf anyways.
      duplicates.maxBy(_.extr.confidence)
    } toSeq
    
    deduplicated
  }
}

object SolrQueryExecutor {
  
  val defaultSolrUrl = "http://knowitall:knowit!@rv-n16.cs.washington.edu:9321/solr"
    
  lazy val defaultInstance = new SolrQueryExecutor(defaultSolrUrl) 
}