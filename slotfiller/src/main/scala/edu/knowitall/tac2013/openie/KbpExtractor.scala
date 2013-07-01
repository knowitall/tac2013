package edu.knowitall.tac2013.openie

import edu.knowitall.common.Resource
import edu.knowitall.common.Timing
import edu.knowitall.srlie.SrlExtractor
import edu.knowitall.srlie.SrlExtractionInstance
import edu.knowitall.srlie.confidence.SrlConfidenceFunction
import edu.knowitall.chunkedextractor.Relnoun
import edu.knowitall.tool.parse.graph.DependencyGraph
import edu.knowitall.tool.conf.impl.LogisticRegression
import edu.knowitall.tool.stem.MorphaStemmer
import java.util.concurrent.atomic.AtomicInteger
import java.io.PrintStream
import scopt.OptionParser
import edu.knowitall.tac2013.prep.ParsedKbpSentence
import edu.knowitall.tac2013.prep.KbpSentenceParser


abstract class KbpExtractor {

  protected val errorCounter = new AtomicInteger(0)

  def extract(parsedSentence: ParsedKbpSentence): Seq[KbpExtraction]
}

object KbpExtractor {
  
  private val extractionsProcessed = new AtomicInteger(0)
  private val sentencesProcessed = new AtomicInteger(0)
  
  object Settings {
    var inputFile = "stdin"
    var outputFile = "stdout"
    var parallel = false
    var limitOpt = Option.empty[Int]
    var inputRaw = false
    var corpus = ""
  }
  
  def main(args: Array[String]): Unit = {
    
    val parser = new OptionParser("KbpExtractor") {
      opt("inputFile", "Raw XML or ParsedKbpSentences for input, default stdinput", { s => Settings.inputFile = s })
      opt("outputFile", "KbpExtractionInstances output file, default stdout", { s => Settings.outputFile = s})    
      opt("inputRaw", "Input raw XML instead of ParsedKbpSentences.", { Settings.inputRaw = true})
      opt("corpus", "Corpus {web, news, forum} for raw input option.", { s => Settings.corpus = s })
      intOpt("limit", "Max extractions to output", { i => Settings.limitOpt = Some(i) })
    }
    
    if (!parser.parse(args)) return
    if (Settings.inputRaw && Settings.corpus.isEmpty()) {
      System.err.println("Error: Must specify --corpus when using --inputRaw")
      return
    }
    
    val time = Timing.time {
      def getInput = if (Settings.inputFile.equals("stdin")) io.Source.stdin else io.Source.fromFile(Settings.inputFile)
      def getOutput = if (Settings.outputFile.equals("stdout")) System.out else new PrintStream(Settings.outputFile)
      Resource.using(getInput) { input =>
        Resource.using(getOutput) { output =>
          run(input.getLines, output)
        }
      }
    }

    val seconds = Timing.Seconds.format(time)
    System.out.flush()
    System.err.flush()
    System.err.println(
        "%d sentences, %d extractions processed in %s".format(
            sentencesProcessed.get, extractionsProcessed.get, seconds))
  }

  private val batchSize = 100

  def run(inputLines: Iterator[String], output: PrintStream): Unit = {
    val extractor = new KbpCombinedExtractor()
    val parsedSentences = {
      if (Settings.inputRaw) 
        KbpSentenceParser.processXml(inputLines, Settings.corpus) 
      else 
        inputLines flatMap { s => ParsedKbpSentence.read(s) }
    }
    val outStrings = parsedSentences.grouped(batchSize).flatMap { batch =>
      val insts = batch.par.flatMap { sent =>
        val extrs = extractor.extract(sent)
        sentencesProcessed.incrementAndGet()
        extractionsProcessed.addAndGet(extrs.size)
        bleat()
        extrs
      }
      insts map KbpExtraction.write
    }
    val limited = Settings.limitOpt match {
      case Some(limit) => outStrings.take(limit)
      case None => outStrings

    }
    limited foreach output.println
  }
  
  def bleat() {
    if (sentencesProcessed.get % 10000 == 0) {
      System.err.println(s"${sentencesProcessed.get} sentences processed, yielding ${extractionsProcessed.get} extractions.")
    }
  }
}

class KbpCombinedExtractor(
  val kbpSrl: KbpSrlExtractor = new KbpSrlExtractor(),
  val kbpRelnoun: KbpRelnounExtractor = new KbpRelnounExtractor()) extends KbpExtractor {

  def extract(parsedSentence: ParsedKbpSentence): Seq[KbpExtraction] = {
    kbpSrl.extract(parsedSentence) ++ kbpRelnoun.extract(parsedSentence)
  }
}

class KbpRelnounExtractor(val relnoun: Relnoun = new Relnoun()) extends KbpExtractor {

  override def extract(parsedSentence: ParsedKbpSentence): Seq[KbpExtraction] = {

    val chunked = parsedSentence.chunkedTokens
    val lemmatized = chunked map MorphaStemmer.lemmatizeToken

    val relnounExtractionInstances = try {
      relnoun.extract(lemmatized).toSeq
    } catch {
      case e: Throwable => {
        System.err.println(
          "Relnoun error #%d on input: %s".format(errorCounter.incrementAndGet(), parsedSentence.dgraph.text))
        Seq.empty
      }
    }

    relnounExtractionInstances.map { inst =>
      KbpExtraction.fromRelnounInstance(inst, parsedSentence)
    }
  }
}

class KbpSrlExtractor(
  val srl: SrlExtractor = new SrlExtractor(),
  val confFunc: LogisticRegression[SrlExtractionInstance] = SrlConfidenceFunction.loadDefaultClassifier())
  extends KbpExtractor {

  override def extract(parsedSentence: ParsedKbpSentence): Seq[KbpExtraction] = {

    val graph = parsedSentence.dgraph

    val srlInstances = try {
      srl.apply(graph)
    } catch {
      case e: Throwable =>
        System.err.println(
          "SrlExtractor error #%d parsing input: %s".format(errorCounter.incrementAndGet(), parsedSentence.dgraph.text))
        Seq.empty
    }

    val kbpExtractions = srlInstances.flatMap { inst =>
      KbpExtraction.fromSrlInstance(inst, parsedSentence, confFunc)
    }

    kbpExtractions
  }
}

