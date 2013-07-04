package edu.knowitall.tac2013.findSlotFillersApp

import scala.io.Source
import edu.knowitall.tac2013.findSlotFillersApp.KBPQueryEntityType._

object SlotTypes {
  private var personSlotTypesSet = Set[String]()
  private var organizationSlotTypesSet = Set[String]()
  private var personSlotTypesList = List[String]()
  private var organizationSlotTypesList = List[String]()

  //private val personSource = Source.fromURL(getClass.getResource("/edu/knowitall/tac2013/findSlotFillersApp/PersonSlotTypes.txt"))
  val personSource = {
    val resourcePath = "/edu/knowitall/tac2013/findSlotFillersApp/PersonSlotTypes.txt"
    val url = getClass.getResource(resourcePath)
    require(url != null, "Could not find resource: " + resourcePath)
    Source.fromURL(url)
  }
  val organizationSource = {
    val resourcePath = "/edu/knowitall/tac2013/findSlotFillersApp/OrganizationSlotTypes.txt"
    val url = getClass.getResource(resourcePath)
    require(url != null, "Could not find resource: " + resourcePath)
    Source.fromURL(url)
  }
  personSource.getLines.foreach(l => {
    if (l.trim().contains("per:")) {
      personSlotTypesSet = personSlotTypesSet + l.trim()
      personSlotTypesList = personSlotTypesList ::: List(l.trim())
    }
  })
  organizationSource.getLines.foreach(l => {
    if (l.trim().contains("org:")) {
      organizationSlotTypesSet = organizationSlotTypesSet + l.trim()
      organizationSlotTypesList = organizationSlotTypesList ::: List(l.trim())
    }
  })

  def getPersonSlotTypesSet = personSlotTypesSet
  def getPersonSlotTypesList = personSlotTypesList
  def getOrganizationSlotTypesSet = organizationSlotTypesSet
  def getOrganizationSlotTypesList = organizationSlotTypesList

  def getSlotTypesList(kbpQueryEntityType: KBPQueryEntityType) = {
    kbpQueryEntityType match {
      case ORG => organizationSlotTypesList
      case PER => personSlotTypesList
    }
  }
}

