package edu.knowitall.tac2013.solr.query

import jp.sf.amateras.solr.scala._
import edu.knowitall.tac2013.app.FilterSolrResults.filterResults
import edu.knowitall.tac2013.openie.KbpExtraction
import edu.knowitall.tac2013.app.Candidate
import edu.knowitall.tac2013.app.Slot
import edu.knowitall.tac2013.app.KBPQuery
import edu.knowitall.tac2013.app.SlotPattern
import edu.knowitall.taggers.Type
import scala.Option.option2Iterable
import edu.knowitall.tac2013.app.SemanticTaggers.getTagTypes

class SolrQueryExecutor(val solrClient: SolrClient, val corefOn: Boolean) {
  
  val queryCounter = new java.util.concurrent.atomic.AtomicInteger
  
  def this(url: String) = this(new SolrClient(url),false)
  def this(url: String, corefOn: Boolean) = this(new SolrClient(url), corefOn)
  
  
  private def issueSolrQuery(queryString: String): Seq[KbpExtraction] = {
    
    // issue query
    val query = solrClient.query(queryString)
    val result = query.sortBy("confidence",Order.desc).rows(15000).getResultAsMap()

    println(s"Ran Query: ($queryString) and got ${result.numFound} hits.")
    
    // load as KbpExtraction
    val kbpExtrs = result.documents.flatMap { doc =>
      val fieldMap = doc.asInstanceOf[Map[String, Any]]
      KbpExtraction.fromFieldMap(fieldMap)
      
    }
    kbpExtrs
  }
  
  private def wrapWithCandidate(kbpSolrQuery: SolrQuery, kbpExtrs: Seq[KbpExtraction]): Seq[Candidate] = {
    kbpExtrs.map { extr =>
      new Candidate(queryCounter.getAndIncrement, kbpSolrQuery, extr, 
          getTagTypes(extr,kbpSolrQuery.pattern))
    }
  }

  private def deduplicate(candidates: Seq[Candidate]) = {
    candidates.groupBy(_.deduplicationKey).map {
      case (key, duplicates) =>
        duplicates.maxBy(_.extr.confidence)
    } toSeq
  }
  
  //takes entity string and map from KBP slot strings to Open IE relation strings and runs queries
  //to our solr instance for every type of OpenIERelation, this method uses no filters, this is for debugging purposes
  def executeUnfilteredQuery(kbpQuery: KBPQuery, slot: Slot): Seq[Candidate] = {

    val patterns = slot.patterns
    
    val solrQueries = patterns.flatMap { pattern => 
      val queryBuilder = new SolrQueryBuilder(pattern, kbpQuery,corefOn)
      queryBuilder.getQueries 
    }
    
    // group solr queries by their query string to avoid running the same solr query multiple times.
    val distinctQueries = solrQueries.groupBy(_.queryString).iterator.toSeq
    
    // map (queryString, Seq[SolrQuery]) to (Seq[KbpExtraction], Seq[SolrQuery])
    
    val kbpExtracionResults = distinctQueries.par.map({ case (qstring, solrQueries) => (issueSolrQuery(qstring), solrQueries) }).toList
    
    val candidates = kbpExtracionResults.flatMap { case (kbpExtrs, solrQueries) =>
      solrQueries.flatMap { sq =>
        deduplicate(wrapWithCandidate(sq, kbpExtrs))
      }
    }
    candidates
  }
}

object SolrQueryExecutor {
  
  val oldUrl = "http://knowitall:knowit!@rv-n16.cs.washington.edu:9321/solr"

  val newUrl = "http://knowitall:knowit!@rv-n16.cs.washington.edu:8123/solr"

  private lazy val oldCorpus = new SolrQueryExecutor(oldUrl)
  private lazy val oldCorpusCoref = new SolrQueryExecutor(oldUrl, corefOn=true)
 
  private lazy val newCorpus = new SolrQueryExecutor(newUrl)
  private lazy val newCorpusCoref = new SolrQueryExecutor(newUrl, corefOn=true)
  


  
  def getInstance(str: String, corefOn: Boolean = false): SolrQueryExecutor = (str, corefOn) match {
    case ("old", true) => oldCorpusCoref
    case ("old", false) => oldCorpus
    case ("new", true) => newCorpusCoref
    case ("new", false) => newCorpus
    case _ => throw new IllegalArgumentException("Corpus must be either \"old\" or \"new\"")
  }
}