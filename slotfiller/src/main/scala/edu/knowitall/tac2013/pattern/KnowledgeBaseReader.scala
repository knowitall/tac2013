package edu.knowitall.tac2013.pattern

import java.io.File
import edu.knowitall.common.Resource.using

/**
 * Reads KbElements from the TAC 2009 knowledge base.
 */
class KnowledgeBaseReader(val path: File) extends Iterable[KbElement] {

  def iterator = path.listFiles.iterator.grouped(10).flatMap(group => group.par.flatMap(file => KnowledgeBaseReader.readXml(file)))
}

object KnowledgeBaseReader {
  
  import scala.xml.XML
  
  val wikiNameRegex = """(.*)(\s+\(.+\)\s*)?""".r

  def clean(str: String): String = str.replaceAll("\n", " ")

  def readXml(file: File): Iterator[KbElement] = {

    val root = XML.loadFile(file)
    val entities = root.\("entity")
    entities.iterator.flatMap { entity =>
      val nameNode = entity.attribute("name").get.head
      val name = nameNode.text match {
        case wikiNameRegex(realName, _) => realName
        case _ => throw new Exception("Should have been able to match: " + nameNode.text)
      }
      val id = entity.attribute("id").get.head.text
      val entityType = entity.attribute("type").get.head.text
      val factsClass = entity.\("facts").head.attribute("class").get.head.text
      val factNodes = entity.\("facts").\("fact")
      val facts = factNodes map processFactNode(factsClass, entityType)
      facts.iterator map { fact =>
        val entityItem = KbItem(clean(name), Some(id))
        val factItem = KbItem(clean(fact.text), fact.linkId)
        val element = KbElement(entityItem, factItem, entityType, fact.slotNames)
        element
      } filter(e => e.entityType == "ORG" || e.entityType == "PER") filter(_.slotNames.nonEmpty)
    }
  }
  
  case class Fact(val slotNames: Seq[String], val text: String, val linkId: Option[String])
  
  def processFactNode(factClass: String, entityType: String)(factNode: scala.xml.Node): Fact = {
    val factType = factNode.attribute("name").get.head.text
    
    val slotNames = entityType match {
      case "PER" => InfoboxMappings.perSlotLookup(factClass, factType)
      case "ORG" => InfoboxMappings.orgSlotLookup(factClass, factType)
      case _ => Nil
    }
    // Use a fact link only if it is the only link and there is no fact text (due to formatting... otherwise cant trust it.)
    val factText = factNode.text
    val linkNode = factNode.\("link").headOption
    
    if (linkNode.isDefined) {
      val nodeId = linkNode.get.attribute("entity_id") map { attr => attr.head.text }
      Fact(slotNames, linkNode.get.text, nodeId)
    } else if (factText.nonEmpty) Fact(slotNames, factText, None)
    else {
      new Fact(Nil, "DISCARD", None)
    }
  }
  
  def main(args: Array[String]) {
    
    val path = args(0)
    
    new KnowledgeBaseReader(new File(path)) foreach println
  }
}

object InfoboxMappings {
  
  val personMappingResource = "/edu/knowitall/tac2013/pattern/per_infobox_mapping.tab"
  val orgMappingResource = "/edu/knowitall/tac2013/pattern/org_infobox_mapping.tab"
        
  private def loadMap(resource: String): Map[String, Seq[String]] = {
    val url = Option(getClass.getResource(resource)).getOrElse(throw new RuntimeException("Not found: " + resource))
    using(io.Source.fromURL(url)) { source =>
      val splitLines = source.getLines.map { line => line.toLowerCase.replaceAll("""/""", "_").split("\t") }
      val parsedLines = splitLines.map { 
        case Array(reg, generic, _*) => (reg, generic)
        case x => throw new RuntimeException("Parse error loading %s: malformed line %s".format(resource, x.mkString("\t")))
      }
      // group by reg infobox and fixup resulting groupby type.
      parsedLines.toSeq.groupBy(_._1).map { case (key, keyValues) => (key, keyValues.map { case (key, value) => value })}
    }
  }
  
  private val perMap = loadMap(personMappingResource)
  private val orgMap = loadMap(orgMappingResource)
    
  private def lookup(infoboxMap: Map[String, Seq[String]], infoboxClass: String, infoboxName: String): Seq[String] = {
    val key = s"$infoboxClass:$infoboxName".toLowerCase
    infoboxMap.get(key).getOrElse(Nil)
  }
  
  /**
   * Example args: "Infobox Artist", "birthplace"  -->  Seq("per:city_of_birth", "per:country_of_birth, ...)
   */
  def perSlotLookup(kbInfoboxClass: String, kbInfobox: String) = lookup(perMap, kbInfoboxClass, kbInfobox)
  
  def orgSlotLookup(kbInfoboxClass: String, kbInfobox: String) = lookup(orgMap, kbInfoboxClass, kbInfobox)
}