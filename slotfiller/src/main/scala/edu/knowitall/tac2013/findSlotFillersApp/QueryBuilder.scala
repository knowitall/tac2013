package edu.knowitall.tac2013.findSlotFillersApp

import CandidateType._

case class KbpSolrQuery(val queryString: String, val resultType: CandidateType, val pattern: SlotPattern)

class QueryBuilder(val pattern: SlotPattern, val entityName: String, val nodeId: Option[String]) {

  private def getQueryString(fields: Seq[String]) = {
    val nonEmptyFields = fields.filter(_.nonEmpty)

    nonEmptyFields.mkString(" AND ")
  }

  lazy val arg1TextConstraint: Option[String] = {
    pattern.entityIn match {
      case Some("arg1") => Some("+arg1Text:\"%s\"".format(entityName))
      case _ => None
    }
  }

  lazy val relTextConstraint: Option[String] = {
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

  lazy val arg2TextConstraint: Option[String] = {
    pattern.entityIn match {
      case Some("arg2") => Some("+arg2Text:\"%s\"".format(entityName))
      case _ => None
    }
  }

  lazy val arg2StartConstraint: Option[String] = {
    pattern.arg2Begins match {
      case Some(arg2Begins) => Some("+arg2Text:\"%s\"".format(arg2Begins))
      case None => None
    }
  }

  lazy val arg1LinkConstraint: Option[String] = {
    (pattern.entityIn, nodeId) match {
      case (Some("arg1"), Some(id)) => Some("+arg1WikiLinkNodeId:\"%s\"".format(id))
      case _ => None
    }
  }

  lazy val arg2LinkConstraint: Option[String] = {
    (pattern.entityIn, nodeId) match {
      case (Some("arg2"), Some(id)) => Some("+arg2WikiLinkNodeId:\"%s\"".format(id))
      case _ => None
    }
  }

  lazy val getQueries: Seq[KbpSolrQuery] = {

    buildQuery.toSeq ++ buildLinkedQuery
  }

  def buildQuery: Option[KbpSolrQuery] = {

    if (!pattern.isValid) {
      None
    } else {
      val queryFields = Seq(arg1TextConstraint, arg2TextConstraint, relTextConstraint, arg2StartConstraint).flatten
      val query = KbpSolrQuery(getQueryString(queryFields), CandidateType.REGULAR, pattern)
      Some(query)
    }
  }

  def buildLinkedQuery: Option[KbpSolrQuery] = {

    if (!pattern.isValid || nodeId.isEmpty) {
      None
    } else {
      val queryFields = Seq(arg1LinkConstraint, arg2LinkConstraint, relTextConstraint, arg2StartConstraint).flatten
      val query = KbpSolrQuery(getQueryString(queryFields), CandidateType.LINKED, pattern)
      Some(query)
    }
  }
}