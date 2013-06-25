package edu.knowitall.tac2013.openie

import edu.knowitall.srlie.SrlExtractionInstance
import edu.knowitall.collection.immutable.Interval
import edu.knowitall.tool.parse.graph.DependencyGraph
import edu.knowitall.chunkedextractor.BinaryExtractionInstance
import edu.knowitall.chunkedextractor.Relnoun
import edu.knowitall.srlie.confidence.SrlConfidenceFunction.SrlConfidenceFunction
import edu.knowitall.tool.chunk.ChunkedToken
import edu.knowitall.chunkedextractor.ExtractionPart
import edu.knowitall.tac2013.prep.ParsedKbpSentence
import edu.knowitall.srlie.SrlExtraction

abstract class KbpRelation {
  def tokenInterval: Interval
  def originalText: String
  def tokens: Seq[ChunkedToken]
}
object KbpRelation {
  
  val NUM_FIELDS = 2
  
  /** 
  * COLUMNS:  
  * INTERVAL | ORIGINAL_TEXT
  */ 
  def writeHelper(rel: KbpRelation): Seq[String] = {
    val intervalString = "%d %d".format(rel.tokenInterval.start, rel.tokenInterval.last)
    Seq(intervalString, rel.originalText)
  }
  
  def readHelper(fields: Seq[String], sentence: ParsedKbpSentence): Option[KbpRelation] = {
    fields match {
      case Seq(interval, original, _*) => {
        readHelper(interval, original, sentence)
      }
      case _ => None
    }
  }
  
  import KbpArgument.intervalRegex
  
  private def readHelper(intervalString: String, originalTextString: String, sentence: ParsedKbpSentence): Option[KbpRelation] = {
    
    intervalString match {
      case intervalRegex(start, last) => {
        Some(new KbpRelation() {
          val tokenInterval = Interval.closed(start.toInt, last.toInt)
          val originalText = originalTextString
          def tokens = sentence.chunkedTokens.drop(tokenInterval.start).take(tokenInterval.length)
        })
      }
      case _ => None
    }
  }
  
  def fromSrlRelation(rel: SrlExtraction.Relation, sentence: ParsedKbpSentence) = new KbpRelation() {
    val tokenInterval = rel.span
    val originalText = rel.text
    val tokens = sentence.chunkedTokens.drop(tokenInterval.start).take(tokenInterval.length)
  }
  def fromRelnounRelation(rel: ExtractionPart[Relnoun.Token]) = new KbpRelation() {
    val tokenInterval = rel.interval
    val originalText = rel.text
    val tokens = rel.tokens
  }
}

abstract class KbpArgument {
  def tokenInterval: Interval
  def originalText: String
  def tokens: Seq[ChunkedToken]
  def wikiLink: Option[String]
  def types: Seq[String] // Seq[Type]?
}

object KbpArgument {
  
  def fromSrlArgument(arg: SrlExtraction.Argument, sentence: ParsedKbpSentence) = new KbpArgument() {
    val tokenInterval = arg.tokenInterval
    val originalText = arg.text
    val tokens = sentence.chunkedTokens.drop(tokenInterval.start).take(tokenInterval.length)
    val wikiLink = None
    val types = Nil
  }
  def fromRelnounArgument(arg: ExtractionPart[Relnoun.Token]) = new KbpArgument() {
    val tokenInterval = arg.interval
    val originalText = arg.text
    val tokens = arg.tokens
    val wikiLink = None
    val types = Nil
  }

  val NUM_FIELDS = 4
  
  /**
   * COLUMNS:
   * INTERVAL | ORIGINAL_TEXT | WIKILINK | TYPES
   */
  def writeHelper(arg: KbpArgument): Seq[String] = {
    val intervalString = "%d %d".format(arg.tokenInterval.start, arg.tokenInterval.last)
    val wikiLinkString = arg.wikiLink.getOrElse("")
    val typeString = arg.types.mkString(" ")
    Seq(intervalString, arg.originalText, wikiLinkString, typeString)
  }
  
  def readHelper(fields: Seq[String], sentence: ParsedKbpSentence): Option[KbpArgument] = {
    fields match {
      case Seq(interval, original, wikiLink, types, _*) => {
        readHelper(interval, original, wikiLink, types, sentence)
      }
      case _ => None
    }
  }
  
  val intervalRegex = "^([0-9]+)\\s+([0-9]+)$".r
  
  private def readHelper(
      intervalString: String, 
      originalTextString: String, 
      wikiLinkString: String, 
      typesString: String,
      sentence: ParsedKbpSentence): Option[KbpArgument] = {
    
    intervalString match {
      case intervalRegex(start, last) => {
        Some(new KbpArgument() {
          val tokenInterval = Interval.closed(start.toInt, last.toInt)
          val originalText = originalTextString
          val wikiLink = if (wikiLinkString.isEmpty()) None else Some(wikiLinkString)
          val types = typesString.split(" ").toSeq
          val tokens = sentence.chunkedTokens.drop(tokenInterval.start).take(tokenInterval.length)
        })
      }
      case _ => None
    }
  }
}

class KbpExtraction(
  val arg1: KbpArgument,
  val rel: KbpRelation,
  val arg2: KbpArgument,
  val confidence: Double,
  val extractor: String,
  val sentence: ParsedKbpSentence
)

object KbpExtraction {
  
  val errorCounter = new java.util.concurrent.atomic.AtomicInteger(0)
  
  val tabRegex = "\t".r
  
  def fromFieldMap(fieldMap: Map[String, String]) = solr.KbpExtractionConverter.fromFieldMap(fieldMap)
  
  def write(extr: KbpExtraction): String = {
    
    val arg1Strings = KbpArgument.writeHelper(extr.arg1)
    val relStrings = KbpRelation.writeHelper(extr.rel)
    val arg2Strings = KbpArgument.writeHelper(extr.arg2)
    val confidence = "%.04f".format(extr.confidence)
    val extractor = extr.extractor
    val sentence = ParsedKbpSentence.write(extr.sentence)

    // Concatenate them all with tabs
    val allFields = arg1Strings ++ relStrings ++ arg2Strings ++ Seq(confidence, extractor, sentence)
    
    allFields.mkString("\t")
      
  }
  
  def read(str: String): Option[KbpExtraction] = {
    
    val split = tabRegex.split(str)
    // Section split by (arg1, rel, arg2, confidence, extractor, sentence...)    
    val arg1Split = split.take(KbpArgument.NUM_FIELDS)
    val relSplit = split.drop(KbpArgument.NUM_FIELDS).take(KbpRelation.NUM_FIELDS)
    val arg2Split = split.drop(KbpArgument.NUM_FIELDS + KbpRelation.NUM_FIELDS).take(KbpArgument.NUM_FIELDS)
    val confidence = split.drop(2 * KbpArgument.NUM_FIELDS + KbpRelation.NUM_FIELDS).take(1).headOption
    val extractor = split.drop(2 * KbpArgument.NUM_FIELDS + KbpRelation.NUM_FIELDS + 1).take(1).headOption
    val sentence = split.drop(2 * KbpArgument.NUM_FIELDS + KbpRelation.NUM_FIELDS + 2)
    
    val tryToParse = readHelper(arg1Split, relSplit, arg2Split, confidence, extractor, sentence)

    tryToParse match {
      case Some(kbpExtraction) => tryToParse
      case None => {
        val errNo = errorCounter.incrementAndGet()
        val msg = "KbpExtraction error #%d: Couldn't read(str): %s".format(errNo, str)
        System.err.println(msg)
        None
      }
    }
  }
    
  private def readHelper(
        arg1Split: Seq[String], 
        relSplit: Seq[String], 
        arg2Split: Seq[String], 
        confString: Option[String], 
        extrString: Option[String], 
        sentSplit: Seq[String]): Option[KbpExtraction] = {
      
      // if we came up short on fields at all, then sentence will be short.
    if (sentSplit.length < ParsedKbpSentence.NUM_FIELDS) {
      val errNo = errorCounter.incrementAndGet()
      val msg = "KbpExtraction error #%d: Wrong number of sentence fields(%d): %s".format(errNo, sentSplit.length, sentSplit.mkString("\n"))
      System.err.println(msg)
      None
    } else {
      val sentOpt = ParsedKbpSentence.read(sentSplit)
      val arg1Opt = sentOpt flatMap { s => KbpArgument.readHelper(arg1Split, s) }
      val relOpt  = sentOpt flatMap { s => KbpRelation.readHelper(relSplit, s) }
      val arg2Opt = sentOpt flatMap { s => KbpArgument.readHelper(arg2Split, s) }
      val confOpt = confString.map(_.toDouble)
      val extrOpt = extrString
      
          
      // If all parsed correctly, then we can .get them all.
      val parsedFields = Seq(arg1Opt, relOpt, arg2Opt, confOpt, extrOpt, sentOpt)
      if (parsedFields.length == parsedFields.flatten.length) {
        Some(new KbpExtraction(arg1Opt.get, relOpt.get, arg2Opt.get, confOpt.get, extrOpt.get, sentOpt.get))
      } else {
        None
      }
    }
  }
  
  def fromRelnounInstance(
      relnounInst: BinaryExtractionInstance[Relnoun.Token], 
      parsedSentence: ParsedKbpSentence): KbpExtraction = {
    
    val extr = relnounInst.extr
    
    new KbpExtraction(
        arg1 = KbpArgument.fromRelnounArgument(extr.arg1),
        rel = KbpRelation.fromRelnounRelation(extr.rel),
        arg2 = KbpArgument.fromRelnounArgument(extr.arg2),
        confidence = 0.9, 
        extractor = "relnoun",
        sentence = parsedSentence)
  }
  
  def fromSrlInstance(
      srlInst: SrlExtractionInstance, 
      parsedSent: ParsedKbpSentence, 
      confFunc: SrlConfidenceFunction): Iterable[KbpExtraction] = {
    
    val triplized = srlInst.triplize(false).filter(_.extr.arg2s.size == 1) // Some could have zero arg2s
    
    val kbpExtractions = triplized.map { triple =>
      val extr = triple.extr
      new KbpExtraction(
        arg1 = KbpArgument.fromSrlArgument(extr.arg1, parsedSent),
        rel = KbpRelation.fromSrlRelation(extr.rel, parsedSent),
        arg2 = KbpArgument.fromSrlArgument(extr.arg2s(0), parsedSent), // requires triplized filter { args.size == 1 } 
        confidence = confFunc.getConf(triple),
        extractor = "srl",
        sentence = parsedSent
      )
    }
    kbpExtractions
  }
}