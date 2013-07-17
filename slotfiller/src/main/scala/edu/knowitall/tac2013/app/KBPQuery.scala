package edu.knowitall.tac2013.app

import edu.knowitall.tac2013.app.KBPQueryEntityType._
import edu.knowitall.tac2013.solr.query.SolrHelper
//import scala.io._
import scala.xml.XML




case class KBPQuery (val id: String, val name: String, val doc: String,
    val begOffset: Int, val endOffset: Int, val entityType: KBPQueryEntityType,
    val nodeId: Option[String], val slotsToFill: Set[Slot]){

  /**
   * Return a new KBPQuery with different slots to fill.
   */
  def withOverrideSlots(slots: Set[Slot]): KBPQuery = this.copy(slotsToFill = slots)
  
  lazy val docIdToSentNumDocIdPairMap = SolrHelper.getDocIDMapToSentNumsForEntityNameAndNodeID(name, nodeId)
  lazy val docIds = docIdToSentNumDocIdPairMap.keySet.toList
}

object KBPQuery {

  import KBPQueryEntityType._

  // fabricate a KBPQuery for testing
  def forEntityName(name: String, entityType: KBPQueryEntityType, nodeId: Option[String] = None, extraPatterns: Seq[SlotPattern] = Nil): KBPQuery = {
    val slots = Slot.getSlotTypesList(entityType).toSet
    val slotsExtraPatterns = Slot.addPatterns(slots, extraPatterns)
    new KBPQuery("TEST", name, "NULL", -1, -1, entityType, nodeId, slotsExtraPatterns)
  }
  
  //parses a single query from an XML file, will not work if there are more than one query in the XML file
  def parseKBPQuery(pathToFile: String): KBPQuery = {
    

    //val pathToXML = Source.fromFile(pathToFile)
    
    val xml = XML.loadFile(pathToFile)
    
    val queryXMLSeq = xml.\("query")
    if(queryXMLSeq.length != 1) {
      throw new IllegalArgumentException("XML file should include only one query")
    }
    
    val queryXML = queryXMLSeq(0)
    val idText = queryXML.attribute("id") match 
    		{case Some(id) if id.length ==1 => id(0).text
    		 case None => throw new IllegalArgumentException("no id value for query in xml doc")
    		}
    val nameText = queryXML.\\("name").text
    val docIDText = queryXML.\\("docid").text
    val begText = queryXML.\\("beg").text
    val begInt = begText.toInt
    val endText = queryXML.\\("end").text
    val endInt = endText.toInt
    val entityTypeText = queryXML.\\("enttype").text
    val entityType = entityTypeText match{
      case "ORG" => ORG
      case "PER" => PER
      case _ => throw new IllegalArgumentException("improper 'enttype' value in xml doc")
    }
    val nodeIDText = queryXML.\\("nodeid").text.trim()
    val nodeId = if (nodeIDText.isEmpty() || nodeIDText.startsWith("NIL")) None else Some(nodeIDText)
    val ignoreText = queryXML.\\("ignore").text
    val ignoreSlots = {
      val ignoreNames = ignoreText.split(" ").toSet
      Slot.getSlotTypesList(entityType).filter(slot => ignoreNames.contains(slot.name))
    }
    
    //find slotsToFill by taking the difference between the global slots set
    // and the set specified in the xml doc
    val slotsToFill = entityType match{
      case ORG => {
        Slot.orgSlots &~ ignoreSlots
      }
      case PER => {
        Slot.personSlots &~ ignoreSlots
      }
    }
    
    

    new KBPQuery(idText,nameText,docIDText,begInt,endInt,entityType,nodeId,slotsToFill)
  }
  
  private def parseSingleKBPQueryFromXML(queryXML: scala.xml.Node): KBPQuery = {
    

    //val pathToXML = Source.fromFile(pathToFile)
    
    val idText = queryXML.attribute("id") match 
    		{case Some(id) if id.length ==1 => id(0).text
    		 case None => throw new IllegalArgumentException("no id value for query in xml doc")
    		}
    val nameText = queryXML.\\("name").text
    val docIDText = queryXML.\\("docid").text
    val begText = queryXML.\\("beg").text
    val begInt = begText.toInt
    val endText = queryXML.\\("end").text
    val endInt = endText.toInt
    val entityTypeText = queryXML.\\("enttype").text
    val entityType = entityTypeText match {
      case "ORG" => ORG
      case "PER" => PER
      case _ => throw new IllegalArgumentException("improper 'enttype' value in xml doc")
    }
    val nodeIDText = queryXML.\\("nodeid").text.trim()
    val nodeId = if (nodeIDText.isEmpty || nodeIDText.startsWith("NIL")) None else Some(nodeIDText)
    val ignoreText = queryXML.\\("ignore").text
    val ignoreSlots = {
      val ignoreNames = ignoreText.split(" ").toSet
      Slot.getSlotTypesList(entityType).filter(slot => ignoreNames.contains(slot.name))
    }
    
    
    //find slotsToFill by taking the difference between the global slots set
    // and the set specified in the xml doc
    val slotsToFill = entityType match{
      case ORG => {
        Slot.orgSlots &~ ignoreSlots
      }
      case PER => {
        Slot.personSlots &~ ignoreSlots
      }
    }
    new KBPQuery(idText,nameText,docIDText,begInt,endInt,entityType,nodeId,slotsToFill)
  }
  

  def parseKBPQueries(pathToFile: String): List[KBPQuery] = {
    
     val xml = XML.loadFile(pathToFile)
     val queryXMLSeq = xml.\("query")
     
     val kbpQueryList = for( qXML <- queryXMLSeq) yield parseSingleKBPQueryFromXML(qXML)
    
     kbpQueryList.toList
  }
  
}