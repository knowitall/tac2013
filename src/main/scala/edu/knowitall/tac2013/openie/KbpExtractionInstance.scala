package edu.knowitall.tac2013.openie

import java.util.concurrent.atomic.AtomicInteger

class KbpExtractionInstance(val extr: KbpExtraction, val sent: ParsedKbpSentence)

object KbpExtractionInstance {
  
  protected val errorCounter = new AtomicInteger(0)
  
  def write(inst: KbpExtractionInstance): String = {
    val extrString = KbpExtraction.write(inst.extr)
    val sentString = ParsedKbpSentence.write(inst.sent)
    "%s\t%s".format(extrString, sentString)
  }
  
  def read(str: String): Option[KbpExtractionInstance] = {
   
    def fail(msg: String) = { System.err.println(msg); None }
    
    val split = str.split("\t")
    
    if (split.length < KbpExtraction.NUM_FIElDS + ParsedKbpSentence.NUM_FIELDS) {
      val error = "KbpExtractionInstance parse error #%d on input: %s".format(errorCounter.incrementAndGet(), str)
      return fail(error)
    }
    
    val extrFields = split.take(KbpExtraction.NUM_FIElDS)
    val sentFields = split.drop(KbpExtraction.NUM_FIElDS)
    
    return KbpExtraction.read(extrFields) match {
      case Some(extr) => {
        ParsedKbpSentence.read(sentFields) match {
          case Some(sent) => Some(new KbpExtractionInstance(extr, sent))
          case None => None
        }
      }
      case None => None
    }
  }
}