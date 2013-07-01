package edu.knowitall.tac2013.openie.solr

import org.apache.solr.common.SolrInputDocument
import edu.knowitall.tac2013.openie._
import edu.knowitall.tac2013.prep.ParsedKbpSentence
import scala.collection.JavaConverters._

/**
 * Helper object for converting between KbpExtractions and SolrDocuments
 */
object KbpExtractionConverter {
  
  private val errorCounter = new java.util.concurrent.atomic.AtomicInteger(0)

  val kbpExtractionFields = Set(
    // arg1 fields
    "arg1Text",
    "arg1Interval",
    "arg1WikiLinkName",
    "arg1WikiLinkFbid",
    "arg1WikiLinkNodeId",
    "arg1Types",
    
    // rel fields
    "relText",
    "relInterval",
    "relTypes",

    // arg2 fields
    "arg2Text",
    "arg2Interval",
    "arg2WikiLinkName",
    "arg2WikiLinkFbid",
    "arg2WikiLinkNodeId",
    "arg2Types",

    // per-extraction fields
    "confidence",
    "extractor",
    
    // sentence fields
    "docId",
    "sentOffset",
    "sentNum",
    "chunks",
    "dgraph")
    
  def fromFieldMap(fieldMap: Map[String, Any]): Option[KbpExtraction] = {
    
    if (!kbpExtractionFields.subsetOf(fieldMap.keySet)) {
      val missingFields = kbpExtractionFields.filter(f => !fieldMap.contains(f))
      val msgFmt = "KbpExtractionConverter error #%d: Missing fields [%s]"
      val msg = msgFmt.format(errorCounter.incrementAndGet(), missingFields.mkString(", "))
      System.err.println(msg)
      None
    } else {
      
      val sentenceFields = Seq("docId", "sentNum", "sentOffset", "chunks", "dgraph").map(fieldMap(_).asInstanceOf[String])
      
      ParsedKbpSentence.read(sentenceFields) flatMap { sentence =>
        
        val arg1WikiLinkName = fieldMap("arg1WikiLinkName").asInstanceOf[String]
        val arg1WikiField = if (!arg1WikiLinkName.isEmpty) {
          s"${fieldMap("arg1WikiLinkName")} ${fieldMap("arg1WikiLinkFbid")} ${fieldMap("arg1WikiLinkNodeId")}"
        } else ""
        
        val arg1Fields = Seq(
            fieldMap("arg1Interval"), 
            fieldMap("arg1Text"),  
            arg1WikiField, 
            fieldMap("arg1Types")).map(_.asInstanceOf[String])
            
        val relFields = Seq(
            "relInterval", 
            "relText", 
            "relTypes").map(fieldMap(_).asInstanceOf[String])
           
        val arg2WikiLinkName = fieldMap("arg2WikiLinkName").asInstanceOf[String]
        val arg2WikiField = if (!arg2WikiLinkName.isEmpty) {
          s"${fieldMap("arg2WikiLinkName")} ${fieldMap("arg2WikiLinkFbid")} ${fieldMap("arg2WikiLinkNodeId")}"
        } else ""          
            
        val arg2Fields = Seq(
            fieldMap("arg2Interval"), 
            fieldMap("arg2Text"),  
            arg2WikiField, 
            fieldMap("arg2Types")).map(_.asInstanceOf[String])
            
        val arg1Opt = KbpArgument.readHelper(arg1Fields, sentence)
        val relOpt = KbpRelation.readHelper(relFields, sentence)
        val arg2Opt = KbpArgument.readHelper(arg2Fields, sentence)
        if (arg1Opt.isEmpty || relOpt.isEmpty || arg2Opt.isEmpty) {
          val msg = "KbpExtractionConverter error #%d: Error parsing args or rel.".format(errorCounter.incrementAndGet())
          System.err.println(msg)
          None
        } else {
          val confidence = fieldMap("confidence").asInstanceOf[Double]
          val extractor = fieldMap("extractor").asInstanceOf[String]
          Some(new KbpExtraction(
            arg1 = arg1Opt.get,
            rel = relOpt.get,
            arg2 = arg2Opt.get,
            confidence = confidence,
            extractor = extractor,
            sentence = sentence))
        }
      }
    }
  }
  
  def toSolrInputDocument(extr: KbpExtraction): SolrInputDocument = {
    
    // Prepare fields as strings
    val arg1 = extr.arg1
    val rel = extr.rel
    val arg2 = extr.arg2
    val sent = extr.sentence

    val arg1Text = extr.arg1.originalText
    val arg1Interval = "%d %d".format(arg1.tokenInterval.start, arg1.tokenInterval.last)
    val arg1WikiLinkName = arg1.wikiLink.map(_.name).getOrElse("")
    val arg1WikiLinkFbid = arg1.wikiLink.map(_.fbid).getOrElse("")
    val arg1WikiLinkNodeId = arg1.wikiLink.flatMap(_.nodeId).getOrElse("")
    val arg1Types = arg1.types.mkString(" ")
    
    val relText = extr.rel.originalText
    val relInterval = "%d %d".format(rel.tokenInterval.start, rel.tokenInterval.last)
    val relTypes = extr.rel.types.mkString(" ")
    
    val arg2Text = extr.arg2.originalText
    val arg2Interval = "%d %d".format(arg2.tokenInterval.start, arg2.tokenInterval.last)
    val arg2WikiLinkName = arg2.wikiLink.map(_.name).getOrElse("")
    val arg2WikiLinkFbid = arg2.wikiLink.map(_.fbid).getOrElse("")
    val arg2WikiLinkNodeId = arg2.wikiLink.flatMap(_.nodeId).getOrElse("")
    val arg2Types = arg2.types.mkString(" ")
    
    val confidence = extr.confidence
    val extractor = extr.extractor
    
    val docId = sent.docId
    val sentOffset = sent.startOffset.toString
    val sentNum = sent.sentNum.toString
    val chunks = sent.chunks.mkString(" ")
    val dgraph = sent.dgraph.serialize
    
    // Insert fields into doc
    
    val doc = new SolrInputDocument()
    doc.addField("arg1Text", arg1Text)
    doc.addField("arg1Interval", arg1Interval)
    doc.addField("arg1WikiLinkName", arg1WikiLinkName)
    doc.addField("arg1WikiLinkFbid", arg1WikiLinkFbid)
    doc.addField("arg1WikiLinkNodeId", arg1WikiLinkNodeId)
    doc.addField("arg1Types", arg1Types)
    
    doc.addField("relText", relText)
    doc.addField("relInterval", relInterval)
    doc.addField("relTypes", relTypes)
    
    doc.addField("arg2Text", arg2Text)
    doc.addField("arg2Interval", arg2Interval)
    doc.addField("arg2WikiLinkName", arg2WikiLinkName)
    doc.addField("arg2WikiLinkFbid", arg2WikiLinkFbid)
    doc.addField("arg2WikiLinkNodeId", arg2WikiLinkNodeId)
    doc.addField("arg2Types", arg2Types)
    
    doc.addField("confidence", confidence)
    doc.addField("extractor", extractor)
    
    doc.addField("docId", docId)
    doc.addField("sentOffset", sentOffset)
    doc.addField("sentNum", sentNum)
    doc.addField("chunks", chunks)
    doc.addField("dgraph", dgraph)
    
    assert(kbpExtractionFields.equals(doc.getFieldNames().asScala.toSet))
    
    doc
  }
}