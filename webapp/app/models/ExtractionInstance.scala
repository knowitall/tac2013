package models

import java.util.ArrayList

import scala.collection.JavaConverters._

import edu.knowitall.tac2013.openie._

case class ExtractionInstance(docId: String, arg1: String, rel: String, arg2: String,
    arg1Types: Seq[String], relTypes: Seq[String], arg2Types: Seq[String],
    arg1Postag: String, relPostag: String, arg2Postag: String,
    sentence: String, extractor: String, confidence: Double, count: Int) {
  //(ids: List[String], arg1: String, rel: String, arg2s: Seq[String], arg1Types: Seq[String], relTypes: Seq[String], arg2Types: Seq[String], arg1_postag: String, rel_postag: String, arg2_postag: String, sentence: String, url: String, extractor: String, confidence: Double, count: Int) {
  def arg1s = Seq(arg1)
  def rels = Seq(rel)
  def arg2s = Seq(arg2)
  
  def url = docId

  def arg1String = arg1
  def relString = rel
  def arg2String = arg2

  val parts = Iterable(arg1, rel, arg2)

  def text(groupBy: ExtractionPart) = {
    (groupBy match {
      case Argument1 => Seq(this.relString, this.arg2String)
      case Relation => Seq(this.arg1String, this.arg2String)
      case Argument2 => Seq(this.arg1String, this.relString)
    }).mkString(" ")
  }

  def score(groupBy: ExtractionPart) = {
    val postags = groupBy match {
      case Argument1 => Seq(this.arg2Postag.split(" "))
      case Relation => Seq(this.arg1Postag.split(" "), this.arg2Postag.split(" "))
      case Argument2 => Seq(this.arg1Postag.split(" "))
    }
    val properScore = postags.map { postags =>
      if (postags.find(_ startsWith "NNP").isDefined) 1
      else if (postags.find(_ startsWith "PRP").isDefined) -1
      else 0
    }.sum

    (-properScore, -this.confidence)
  }
}

object ExtractionInstance {
  class ExtractionInstanceOrdering(groupBy: ExtractionPart) extends Ordering[ExtractionInstance] {
    def compare(a: ExtractionInstance, b: ExtractionInstance) =
      implicitly[Ordering[(Int, Double)]].compare(a.score(groupBy), b.score(groupBy))
  }
  
  def fromKbpExtraction(e: KbpExtraction): ExtractionInstance = {
    
    def postags(field: KbpExtractionField) = field.tokens.map(_.postag).mkString(" ")
    def linkTypes(field: KbpExtractionField) = field.wikiLink map { link =>
      s"WikiLink(${link.name}, fbid=${link.fbid}, conf=${link.score}, nodeId=${link.nodeId.getOrElse("Nil")})" 
    } toSeq
    
    ExtractionInstance(e.sentence.docId, e.arg1.originalText, e.rel.originalText, e.arg2.originalText,
        linkTypes(e.arg1), linkTypes(e.rel), linkTypes(e.arg2),
        postags(e.arg1), postags(e.rel), postags(e.arg2), 
        e.sentenceText, e.extractor, e.confidence, 1)
  }

  def fromMap(map: Map[String, Any]) = {
    fromKbpExtraction(KbpExtraction.fromFieldMap(map).getOrElse(throw new RuntimeException("Couldn't deserialize instance from solr: %s".format(map))))
  }
}