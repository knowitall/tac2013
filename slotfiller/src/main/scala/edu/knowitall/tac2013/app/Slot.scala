package edu.knowitall.tac2013.app

import scala.io.Source
import edu.knowitall.tac2013.app.KBPQueryEntityType._
import java.net.URL
import edu.knowitall.common.Resource

case class Slot(name: String, slotType: Option[String], maxResults: Int, patterns: Seq[SlotPattern]) {
  require(name == name.trim)
  
  def isLocation = if(name.contains("city") || name.contains("country") || name.contains("stateorprovince") ||
      name.contains("cities") || name.contains("countries") || name.contains("states")) true else false
  def isCountry = if(name.contains("country")) true else false
  def isCity = if(name.contains("city")) true else false
  def isStateOrProvince = if(name.contains("stateorprovince")) true else false
  def isCountryList = if(name.contains("countries")) true else false
  def isCityList = if(name.contains("cities")) true else false
  def isStateOrProvinceList = if(name.contains("states")) true else false
  def isList = if(maxResults > 1) true else false
  def isDate = if(name.contains("date")) true else false
}

object Slot {
  
  private def requireResource(urlString: String): URL = {
    val url = getClass.getResource(urlString)
    require(url != null, "Could not find resource: " + urlString)
    url
  }
  
  private val personResource = "/edu/knowitall/tac2013/findSlotFillersApp/PersonSlotTypes.txt"
  private def personUrl = requireResource(personResource) 
  private def personPatternUrl = requireResource(SlotPattern.personPatternResource)
  private def organizationResource = "/edu/knowitall/tac2013/findSlotFillersApp/OrganizationSlotTypes.txt"
  private def organizationUrl = requireResource(organizationResource)
  private val organizationPatternUrl = requireResource(SlotPattern.organizationPatternResource)

  private def loadSlots(slotUrl: URL, patternUrl: URL, slotPrefix: String): Set[Slot] = {
    
    Resource.using(Source.fromURL(slotUrl)) { slotSource =>
      Resource.using(Source.fromURL(patternUrl)) { patternSource =>
        // filter and split pattern lines
        val validPatternLines = 
          patternSource.getLines.drop(1).map(_.trim).filter(_.contains(slotPrefix)).filterNot(_.startsWith(","))
          
        val patternFields = validPatternLines.map(_.replace(",", " ,").split(",").map(_.trim)).toStream
        // group by slot name
        val slotPatterns = patternFields.groupBy(_(0))
        slotSource.getLines.filter(_.nonEmpty).map(_.trim).map { slotName =>
          fromNameAndPatterns(slotName, slotPatterns.getOrElse(slotName, Seq.empty))  
        } toSet
      }
    }
  }
  
  private def fromNameAndPatterns(slotString: String, patternFields: Seq[Array[String]]): Slot = {
    
    val headPattern = patternFields.head
    
    val maxValues = headPattern(1).toInt
    // Array(slotName, maxValues, relString, arg2Begins, entityIn, slotFillIn, slotType, _*)
    val slotType = {
      val field = if (headPattern.length >= 7) headPattern(6).trim else ""
      if (field.isEmpty()) None else Some(field)
    }
    
    val patterns = patternFields.flatMap(fields => SlotPattern.read(fields))
    
    Slot(slotString, slotType, maxValues, patterns)
  }
  
  
  lazy val personSlots = loadSlots(personUrl, personPatternUrl, "per:")

  lazy val orgSlots = loadSlots(organizationUrl, organizationPatternUrl, "org:")

  lazy val allSlots = personSlots ++ orgSlots
  
  def fromName(name: String) = 
    allSlots.find(_.name == name).getOrElse { throw new RuntimeException("Invalid slot name: " + name) }
  
  def getSlotTypesList(kbpQueryEntityType: KBPQueryEntityType) = {
    kbpQueryEntityType match {
      case ORG => orgSlots
      case PER => personSlots
    }
  }
}

