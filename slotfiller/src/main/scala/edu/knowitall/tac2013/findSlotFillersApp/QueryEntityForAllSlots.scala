package edu.knowitall.tac2013.findSlotFillersApp

import jp.sf.amateras.solr.scala._
import SingleSolrQueryExecutor.issueSolrQuery
import FilterSolrResults.filterResults
import edu.knowitall.tac2013.openie.KbpExtraction

object QueryEntityForAllSlots {

  //takes entity string and map from KBP slot strings to Open IE relation strings and runs queries
  //to our solr instance for every type of OpenIERelation
  def executeQueryForAllSlots(
    kbpQuery: KBPQuery,
    KBPOpenIERelationMap: Map[String, List[SlotPattern]],
    nodeId: Option[String] = None): Map[String, List[CandidateSet]] = {

    //for every relevant slot 
    val resultsMap = for ((slotname, patterns) <- KBPOpenIERelationMap) yield {

      // aggregate and filter results for every different query formulation
      val resultsList = for (pattern <- patterns) yield {

        val qb = new QueryBuilder(pattern, kbpQuery.name, nodeId) //solr query builder

        val combinedResults = qb.getQueries.flatMap { solrQuery => issueSolrQuery(solrQuery) }

        val filteredResults = filterResults(combinedResults.toList, pattern, kbpQuery.name)

        new CandidateSet(pattern, filteredResults)
      }

      //store list of query formulations and solr results with the string
      //of the slot
      (slotname, resultsList)
    }

    resultsMap
  }

  //takes entity string and map from KBP slot strings to Open IE relation strings and runs queries
  //to our solr instance for every type of OpenIERelation, this method uses no filters, this is for debugging purposes
  def executeQueryForAllSlotsWithoutFilter(
    kbpQuery: KBPQuery,
    KBPOpenIERelationMap: Map[String, List[SlotPattern]],
    nodeId: Option[String] = None): Map[String, List[CandidateSet]] = {

    //for every relevant slot 
    val resultsMap = for ((slotname, patterns) <- KBPOpenIERelationMap) yield {

      // aggregate and filter results for every different query formulation
      val resultsList = for (pattern <- patterns) yield {

        val qb = new QueryBuilder(pattern, kbpQuery.name, nodeId) //solr query builder

        val combinedResults = qb.getQueries.flatMap { query => issueSolrQuery(query) }

        new CandidateSet(pattern, combinedResults.toList)
      }

      //store list of query formulations and solr results with the string
      //of the slot
      (slotname, resultsList)
    }

    resultsMap
  }
}