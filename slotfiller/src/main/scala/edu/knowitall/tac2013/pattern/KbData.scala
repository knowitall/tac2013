package edu.knowitall.tac2013.pattern

case class KbItem(val entity: String, val nodeId: Option[String])

case class KbElement(val entity: KbItem, val fill: KbItem, val entityType: String, val slotname: String)

case class KbQuery(val element: KbElement, val entityArg1: Boolean) {
  def entityArg2 = !entityArg1
  def arg1 = if (entityArg1) element.entity else element.fill
  def arg2 = if (entityArg2) element.entity else element.fill
  
  def constraintFor(fieldName: String, attr: String): String = "%s:\"%s\"".format(fieldName, attr)
  
  def constraintFor(fieldName: String, item: KbItem): String = {
    
    val nameConstraint = constraintFor(fieldName, item.entity)
    val nodeConstraint = item.nodeId map { n => constraintFor(fieldName, n) }
    
    (Seq(nameConstraint) ++ nodeConstraint).mkString(" OR ")
    
  }
  

}