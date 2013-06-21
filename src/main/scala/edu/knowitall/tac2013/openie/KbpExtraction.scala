package edu.knowitall.tac2013.openie

import edu.knowitall.srlie.SrlExtractionInstance
import edu.knowitall.collection.immutable.Interval
import edu.knowitall.tool.parse.graph.DependencyGraph
import edu.knowitall.chunkedextractor.BinaryExtractionInstance
import edu.knowitall.chunkedextractor.Relnoun
import edu.knowitall.srlie.confidence.SrlConfidenceFunction.SrlConfidenceFunction
import edu.knowitall.tool.chunk.ChunkedToken
import edu.knowitall.chunkedextractor.ExtractionPart

case class KbpExtraction(
  val arg1: String,
  val rel: String,
  val arg2: String,
  val arg1postags: String,
  val relpostags: String,
  val arg2postags: String,
  val confidence: String,
  val extractor: String
)

object KbpExtraction {
  
  val NUM_FIElDS = 8
  
  def write(e: KbpExtraction): String = {
    val fields = Seq(e.arg1, e.rel, e.arg2, e.arg1postags, e.relpostags, e.arg2postags, e.confidence, e.extractor)
    fields.map(field => field.replaceAll("\t", " ")).mkString("\t")
  }
 
  def read(str: String): Option[KbpExtraction] = read(str.split("\t"))
  
  def read(split: Array[String]): Option[KbpExtraction] = {
    
    split match {
      case Array(arg1, rel, arg2, arg1postags, relpostags, arg2postags, confidence, extractor, _*) => {
        Some(new KbpExtraction(arg1, rel, arg2, arg1postags, relpostags, arg2postags, confidence, extractor))
      }
      case _ => {
        System.err.println("Error parsing KbpExtraction: %s".format(split.mkString("\t")))
        None
      }
    }
  }
  
  def fromRelnounInstance(
      relnounInst: BinaryExtractionInstance[Relnoun.Token], 
      parsedSentence: ParsedKbpSentence): KbpExtraction = {
    
    val extr = relnounInst.extr
    
    def postags(epart: ExtractionPart[ChunkedToken]) = epart.tokens.map(_.postag).mkString(" ")
    
    new KbpExtraction(
        arg1 = extr.arg1.text,
        rel = extr.rel.text,
        arg2 = extr.arg2.text,
        arg1postags = postags(extr.arg1),
        relpostags = postags(extr.rel), 
        arg2postags = postags(extr.arg2),
        confidence = "0.9",
        extractor = "relnoun")
  }
  
  def fromSrlInstance(
      srlInst: SrlExtractionInstance, 
      parsedSent: ParsedKbpSentence, 
      confFunc: SrlConfidenceFunction): Iterable[KbpExtraction] = {
    
    def postags(interval: Interval, graph: DependencyGraph): String = 
      graph.nodes.drop(interval.start).take(interval.size).map(_.postag).mkString(" ")
    
    val triplized = srlInst.triplize(false).filter(_.extr.arg2s.size == 1) // Some could have zero arg2s
    val kbpExtractions = triplized.map { triple =>
      val extr = triple.extr
      val graph = triple.dgraph
      require(extr.arg2s.size == 1)
      new KbpExtraction(
        arg1 = extr.arg1.text,
        rel = extr.rel.text,
        arg2 = extr.arg2s(0).text,
        arg1postags = postags(extr.arg1.interval, graph),
        relpostags = postags(extr.rel.span, graph),
        arg2postags = postags(extr.arg2s(0).interval, graph),
        confidence = "%.04f".format(confFunc.getConf(triple)),
        extractor = "srl"
      )
    }
    kbpExtractions
  }
}