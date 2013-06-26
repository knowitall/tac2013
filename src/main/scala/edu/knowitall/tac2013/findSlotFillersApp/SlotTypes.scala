package edu.knowitall.tac2013.findSlotFillersApp

import scala.io.Source

object SlotTypes {
  var personSlotTypes = Set[String]()
  var organizationSlotTypes = Set[String]()
  private val personSource = Source.fromURL(getClass.getResource("/edu/knowitall/tac2013/findSlotFillersApp/PersonSlotTypes.txt"))
  private val organizationSource = Source.fromURL(getClass.getResource("/edu/knowitall/tac2013/findSlotFillersApp/OrganizationSlotTypes.txt"))
  personSource.getLines.foreach(l => {
	  if (l.trim().contains("per:")){
	    personSlotTypes = personSlotTypes + l.trim()
	  }
  })
  organizationSource.getLines.foreach(l => {
	  if (l.trim().contains("org:")){
	    organizationSlotTypes = organizationSlotTypes + l.trim()
	  }
  })
}