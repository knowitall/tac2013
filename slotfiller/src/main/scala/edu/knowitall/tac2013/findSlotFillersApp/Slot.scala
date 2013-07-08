package edu.knowitall.tac2013.findSlotFillersApp

import scala.io.Source
import edu.knowitall.tac2013.findSlotFillersApp.KBPQueryEntityType._
import java.net.URL
import edu.knowitall.common.Resource

case class Slot(name: String, slotType: Option[String], maxResults: Int, patterns: Seq[SlotPattern]) {
  require(name == name.trim)
}

object Slot {
  
  private def requireResource(urlString: String): URL = {
    val url = urlString.getClass.getResource(urlString)
    require(url != null, "Could not find resource: " + url)
    url
  }
  
  private val personResource = "/edu/knowitall/tac2013/findSlotFillersApp/PersonSlotTypes.txt"
  private val personUrl = requireResource(personResource) 
  private val personPatternUrl = requireResource(SlotPattern.personPatternResource)
  private val organizationResource = "/edu/knowitall/tac2013/findSlotFillersApp/OrganizationSlotTypes.txt"
  private val organizationUrl = requireResource(organizationResource)
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
  
  
  val personSlots = loadSlots(personUrl, personPatternUrl, "per:")

  val orgSlots = loadSlots(organizationUrl, organizationPatternUrl, "org:")

  val allSlots = personSlots ++ orgSlots
  
  def fromName(name: String) = 
    allSlots.find(_.name == name).getOrElse { throw new RuntimeException("Invalid slot name: " + name) }
  
  def getSlotTypesList(kbpQueryEntityType: KBPQueryEntityType) = {
    kbpQueryEntityType match {
      case ORG => orgSlots
      case PER => personSlots
    }
  }
}

