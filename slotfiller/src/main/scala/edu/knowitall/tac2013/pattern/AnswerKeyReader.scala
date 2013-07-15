package edu.knowitall.tac2013.pattern

import java.io.File
import scala.io.Source

case class EntityInfo(val name: String, val nodeId: Option[String])

class AnswerKeyReader(answerKeyFile: File, queryEntities: Map[String, EntityInfo]) extends Iterable[KbElement] {
  
  def this(answerKeyFile: File, queryFile: File) = this(answerKeyFile, QueryMappingReader.fromXmlQueryFile(queryFile))
  
  def iterator = new AnswerKeyIterator(Source.fromFile(answerKeyFile, "UTF8"), queryEntities)
}

class AnswerKeyIterator(answerKeySource: Source, queryEntities: Map[String, EntityInfo]) extends Iterator[KbElement] {
  
  val lines = answerKeySource.getLines.drop(1) // drop the header row
  
  def hasNext = {
    val linesHasNext = lines.hasNext
    if (!linesHasNext) answerKeySource.close()
    linesHasNext
  }
  
  def next = {
    require(hasNext, "Next on empty iterator.")
    readLine(lines.next)    
  }

  /**
   * Reads a line from the TAC_2010 evaluation annotations tab-delimited file.
   * Columns:
   * filler_id | sf_id | system_id | slot_name | docid | fill_start_char | fill_end_char | filler | just_start_char | just_end_char | justification | norm_response |  equiv_class_id | judgement
   */
  def readLine(str: String): KbElement = {

    str.split("\t") match {
      case Array(filler_id, sf_id, system_id, slot_name, docid, fill_start_char, fill_end_char, filler,
        just_start_char, just_end_char, justification, norm_response, equiv_class_id, judgement, _*) => {
        
        val entityInfo = queryEntities(sf_id)
        val entityType = if (slot_name.startsWith("org:")) "ORG" else "PER"
        val entityItem = KbItem(entityInfo.name, entityInfo.nodeId)
        val fillItem = KbItem(filler, None)
        KbElement(entityItem, fillItem, entityType, Seq(slot_name))
      }
      case _ => throw new RuntimeException("Invalid row (%d cols):\n%s".format(str.split("\t").length, str))
    }
  }
}

object QueryMappingReader {
  
  import scala.xml.XML
  import edu.knowitall.common.Resource.using
  import edu.knowitall.tac2013.app.KBPQuery
  
  def fromXmlQueryFile(queryFile: File): Map[String, EntityInfo] = {
    
    System.err.print("Loading query->entity mapping...")
    
    val kbpQueries = KBPQuery.parseKBPQueries(queryFile.getAbsolutePath())
    
    val queryIdInfos = kbpQueries.map { query =>
      (query.id, EntityInfo(query.name, query.nodeId))
    } toMap
    
    System.err.println("done")
    
    queryIdInfos
  }
}
