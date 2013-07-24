package edu.knowitall.tac2013.app

import edu.knowitall.tac2013.app.KBPQueryEntityType._
import edu.knowitall.tac2013.solr.query.SolrHelper
//import scala.io._
import scala.xml.XML




case class KBPQuery (val id: String, val name: String, val doc: String,
    val begOffset: Int, val endOffset: Int, val entityType: KBPQueryEntityType,
    val nodeId: Option[String], val slotsToFill: Set[Slot]){
    
  var foundFbid: Option[String] = None
  
  /**
   * Return a new KBPQuery with different slots to fill.
   */
  def withOverrideSlots(slots: Set[Slot]): KBPQuery = this.copy(slotsToFill = slots)
  
  lazy val docIdToSentNumDocIdPairMap = SolrHelper.getDocIDMapToSentNumsForEntityNameAndNodeID(name, nodeId)
  lazy val docIds = docIdToSentNumDocIdPairMap.keySet.toList
  val numEntityFbids = nodeId match {
    case Some(str) => 1
    case None => {
      val qstring1 = "+arg1Text:\"%s\"".format(name)
      val arg1Extrs = SolrHelper.solrQueryExecutor.get.issueSolrQuery(qstring1)
      val fbidsArg1 = arg1Extrs.flatMap(_.arg1.wikiLink.map(_.fbid)).toSet
      //val nodeIdsArg1 = arg1Extrs.flatMap(_.arg1.wikiLink.flatMap(_.nodeId)).toSet
      
      val qstring2 = "+arg2Text:\"%s\"".format(name)
      val arg2Extrs = SolrHelper.solrQueryExecutor.get.issueSolrQuery(qstring2)
      val fbidsArg2 = arg2Extrs.flatMap(_.arg2.wikiLink.map(_.fbid)).toSet
      //val nodeIdsArg2 = arg2Extrs.flatMap(_.arg2.wikiLink.flatMap(_.nodeId)).toSet
      
      val allFbids = (fbidsArg1 ++ fbidsArg2)
      //val allNodeIds = nodeIdsArg1 ++ nodeIdsArg2
      if (nodeId.isEmpty) System.err.println(s"Unlinked entity $name links to ${allFbids.size} fbids.")
      if (allFbids.size == 1 && nodeId.isEmpty) foundFbid = Some(allFbids.head)
      allFbids.size
    }
  }
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
  
  private def parseSingleKBPQueryFromXML(queryXML: scala.xml.Node): Option[KBPQuery] = {
    

    //val pathToXML = Source.fromFile(pathToFile)
    try{
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
	    new Some(KBPQuery(idText,nameText,docIDText,begInt,endInt,entityType,nodeId,slotsToFill))
    }
    catch {
      case e: Exception => {
        println(e.getMessage())
        return None
        
      }
    }
  }
  

  def parseKBPQueries(pathToFile: String): List[KBPQuery] = {
    
     val xml = XML.loadFile(pathToFile)
     val queryXMLSeq = xml.\("query")
     
     val kbpQueryList = for( qXML <- queryXMLSeq) yield parseSingleKBPQueryFromXML(qXML)
    
     kbpQueryList.toList.flatten
  }
  
}
