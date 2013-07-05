package edu.knowitall.tac2013.solr.query

import edu.knowitall.tac2013.findSlotFillersApp.QueryType._
import edu.knowitall.tac2013.findSlotFillersApp.QueryType
import edu.knowitall.tac2013.findSlotFillersApp.KBPQuery
import edu.knowitall.tac2013.findSlotFillersApp.SlotPattern
import scala.Option.option2Iterable

case class SolrQuery(val queryString: String, val resultType: QueryType, val pattern: SlotPattern)

class SolrQueryBuilder(val pattern: SlotPattern, val kbpQuery: KBPQuery) {

  val arg1TextConstraint: Option[String] = {
    pattern.entityIn match {
      case Some("arg1") => Some("+arg1Text:\"%s\"".format(kbpQuery.name))
      case _ => None
    }
  }

  val relTextConstraint: Option[String] = {
    pattern.openIERelationString match {
      case Some(relString) => {
        val noJobTitle = relString.replace("<JobTitle>", "")
        if (noJobTitle != "") {
          Some("+relText:\"" + noJobTitle + "\"")
        } else {
          None
        }
      }
      case None => None
    }
  }

  val arg2TextConstraint: Option[String] = {
    pattern.entityIn match {
      case Some("arg2") => Some("+arg2Text:\"%s\"".format(kbpQuery.name))
      case _ => None
    }
  }

  val arg2StartConstraint: Option[String] = {
    pattern.arg2Begins match {
      case Some(arg2Begins) => Some("+arg2Text:\"%s\"".format(arg2Begins))
      case None => None
    }
  }

  val arg1LinkConstraint: Option[String] = {
    (pattern.entityIn, kbpQuery.nodeId) match {
      case (Some("arg1"), Some(id)) => Some("+arg1WikiLinkNodeId:\"%s\"".format(id))
      case _ => None
    }
  }

  val arg2LinkConstraint: Option[String] = {
    (pattern.entityIn, kbpQuery.nodeId) match {
      case (Some("arg2"), Some(id)) => Some("+arg2WikiLinkNodeId:\"%s\"".format(id))
      case _ => None
    }
  }
  
  private def getQueryString(fields: Seq[String]) = {
    
    val nonEmptyFields = fields.filter(_.nonEmpty)
    nonEmptyFields.mkString(" AND ")
  }

  val regularQuery: Option[SolrQuery] = {

    if (!pattern.isValid) {
      None
    } else {
      val queryFields = Seq(arg1TextConstraint, arg2TextConstraint, relTextConstraint, arg2StartConstraint).flatten
      val query = SolrQuery(getQueryString(queryFields), QueryType.REGULAR, pattern)
      Some(query)
    }
  }

  val linkedQuery: Option[SolrQuery] = {

    if (!pattern.isValid || kbpQuery.nodeId.isEmpty) {
      None
    } else {
      val queryFields = Seq(arg1LinkConstraint, arg2LinkConstraint, relTextConstraint, arg2StartConstraint).flatten
      val query = SolrQuery(getQueryString(queryFields), QueryType.LINKED, pattern)
      Some(query)
    }
  }
  
  val getQueries: Seq[SolrQuery] = {

    regularQuery.toSeq ++ linkedQuery
  }
}