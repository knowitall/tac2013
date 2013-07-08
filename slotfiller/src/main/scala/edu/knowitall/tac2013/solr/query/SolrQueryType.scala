package edu.knowitall.tac2013.solr.query


object SolrQueryType extends Enumeration {
  type SolrQueryType = Value
  val REGULAR, LINKED, COREF, INFERRED = Value
}