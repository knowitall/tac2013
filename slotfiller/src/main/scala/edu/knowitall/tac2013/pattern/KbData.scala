package edu.knowitall.tac2013.pattern

import org.apache.solr.client.solrj.util.ClientUtils

case class KbItem(val entity: String, val nodeId: Option[String])

case class KbElement(val entity: KbItem, val fill: KbItem, val entityType: String, val slotNames: Seq[String])

case class KbQuery(val element: KbElement, val entityArg1: Boolean) {
  def entityArg2 = !entityArg1
  def arg1 = if (entityArg1) element.entity else element.fill
  def arg2 = if (entityArg2) element.entity else element.fill
  
  def cleanQuery(queryString: String): String = queryString.split(" ").map(s => ClientUtils.escapeQueryChars(s.replaceAll("\"", ""))).mkString(" ")
  
  def constraintFor(fieldName: String, attr: String): String = "%s:\"%s\"".format(fieldName, cleanQuery(attr))
  
  def nodeConstraint(fieldName: String, item: KbItem) = item.nodeId map { n => constraintFor(fieldName + "WikiLinkNodeId", n) }
  
  def nameConstraint(fieldName: String, item: KbItem): String = constraintFor(fieldName + "Text", item.entity)
  
  def queryString: Option[String] = {
    val fill = element.fill
    val entity = element.entity
    if (entity.nodeId.nonEmpty && fill.nodeId.nonEmpty) {
      val constraints = (nodeConstraint("arg1", arg1) ++ nodeConstraint("arg2", arg2))
      Some(constraints.mkString(" AND "))   
    } else {
      None
    }
  }
}

