package edu.knowitall.tac2013.link

import edu.knowitall.browser.entity.EntityLink
import edu.knowitall.browser.entity.EntityLinker
import edu.knowitall.tac2013.openie.KbpExtraction
import edu.knowitall.tac2013.openie.KbpArgument
import edu.knowitall.tac2013.openie.WikiLink
import java.io.File
import java.io.PrintStream
import edu.knowitall.common.Resource.using
import edu.knowitall.common.Timing
import edu.knowitall.tac2013.prep.util.WikiMappingHelper
import scopt.OptionParser
import java.util.concurrent.atomic.AtomicInteger

/**
 * A class to invoke the openie entity linker on KbpExtractions
 * and fill in their wikiLink field. 
 * 
 * wikiNodeMap is a map from wikipedia name to Node ID as found in the initial kbp knowledge base.
 */
class KbpExtractionLinker private (val linker: EntityLinker, val wikiNodeMap: Map[String, String]) {

  val extractionsProcessed = new AtomicInteger(0)
  
  def linkArg(arg: KbpArgument, context: Seq[String]): Option[EntityLink] = {
    // Skip if arg does not contain a proper noun
    if (arg.tokens.exists(_.isProperNoun)) {
      val argString = arg.originalText
      Option(linker.getBestEntity(argString, context))
    } else {
      None
    }
  }
  
  /**
   * Returns a new KbpExtraction with (optionally) wikiLink fields
   * filled in.
   */
  def linkExtraction(extr: KbpExtraction): KbpExtraction = {
    
    val context = Seq(extr.sentenceText)
    
    val arg1Link = linkArg(extr.arg1, context)
    val arg2Link = linkArg(extr.arg2, context)
    
    def toWikiLink(elink: EntityLink): WikiLink = {
      WikiLink(elink.entity.name, elink.entity.fbid, wikiNodeMap.get(elink.entity.name))
    }

    extractionsProcessed.incrementAndGet()
    if (extractionsProcessed.get % 10000 == 0) {
      System.err.println(s"${extractionsProcessed.get} extractions processed.")
    }
    
    return new KbpExtraction(
        arg1 = arg1Link.map(toWikiLink).map(extr.arg1.withLink(_)).getOrElse(extr.arg1),
        rel = extr.rel,
        arg2 = arg2Link.map(toWikiLink).map(extr.arg2.withLink(_)).getOrElse(extr.arg2),
        confidence = extr.confidence,
        extractor = extr.extractor,
        sentence = extr.sentence
    )
  }
}

object KbpExtractionLinker {
  
  def getKbpLinker(baseDir: File, mapFile: File): KbpExtractionLinker = {

    val linker = new EntityLinker(baseDir)
    val wikiMap = using(io.Source.fromFile(mapFile, "UTF8")) { source =>
      WikiMappingHelper.loadNameToNodeIdMap(source.getLines)
    }
    new KbpExtractionLinker(linker, wikiMap)
  }
  
  def main(args: Array[String]): Unit = {
    
    var baseDir = ""
    var mapFile = ""
    var inputFile = ""
    var output = System.out
    
    val parser = new OptionParser() {
      arg("baseDir", "Linker support file baseDir.", { baseDir = _ })
      arg("mapFile", "Wiki name to Kbp Node Id file.", { mapFile = _})
      arg("inputFile", "KbpExtractions input file.", { inputFile = _ })
      opt("outputFile", "File to which output will be written, default stdout.", { s => output = new PrintStream(s) })
    }
    
    if (!parser.parse(args)) return

    run(baseDir, mapFile, inputFile, output)
  }
  
  private def run(baseDir: String, mapFile: String, inputFile: String, output: java.io.PrintStream): Unit = {
    
    val kbpLinker = getKbpLinker(new File(baseDir), new File(mapFile))
    
    val nsTime = Timing.time {
      val input = io.Source.fromFile(inputFile, "UTF8")
      val unlinkedExtractions = input.getLines flatMap KbpExtraction.read
      val linkedExtractions = unlinkedExtractions map kbpLinker.linkExtraction
      linkedExtractions foreach { extr =>
        output.println(KbpExtraction.write(extr))
      }
      input.close()
      output.close()
    }
    
    System.err.println(s"Processed ${kbpLinker.extractionsProcessed.get} extractions in ${Timing.Seconds.format(nsTime)}.")
  }
}