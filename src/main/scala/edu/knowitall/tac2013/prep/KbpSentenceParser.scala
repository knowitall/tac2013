package edu.knowitall.tac2013.prep
import edu.knowitall.tool.chunk.OpenNlpChunker
import edu.knowitall.tool.postag.OpenNlpPostagger
import edu.knowitall.tool.tokenize.OpenNlpTokenizer
import edu.knowitall.tool.parse.ClearParser
import edu.knowitall.tool.postag.ClearPostagger
import scopt.OptionParser
import scala.io.Source
import java.io.PrintStream
import edu.knowitall.common.Resource.using
import edu.knowitall.common.Timing
import java.util.concurrent.atomic.AtomicInteger
import scala.Option.option2Iterable

class KbpSentenceParser() {

  val chunkerModel = OpenNlpChunker.loadDefaultModel
  val postagModel = OpenNlpPostagger.loadDefaultModel
  val tokenModel = OpenNlpTokenizer.loadDefaultModel
  val chunkerLocal = new ThreadLocal[OpenNlpChunker] {
    override def initialValue = {
      val tokenizer = new OpenNlpTokenizer(tokenModel)
      val postagger = new OpenNlpPostagger(postagModel, tokenizer)
      new OpenNlpChunker(chunkerModel, postagger)
    }
  }
  
  lazy val parser = new ClearParser(new ClearPostagger())
  
  def isValid(kbpSentence: KbpSentence): Boolean = {
    kbpSentence.text.length <= 750
  }
  
  def parseKbpSentence(kbpSentence: KbpSentence): Option[ParsedKbpSentence] = {
    
    if (!isValid(kbpSentence)) return None
    try {
    // chunks, then parse
    val chunker = chunkerLocal.get
    val chunked = chunker.chunk(kbpSentence.text) 
    val dgraph = parser.dependencyGraph(kbpSentence.text).serialize
    val tokens = chunked.map(_.string).mkString(" ")
    val postags = chunked.map(_.postag).mkString(" ")
    val chunks = chunked.map(_.chunk).mkString(" ")
    val offsets = chunked.map(_.offset).mkString(" ")
    val startOffset = kbpSentence.offset.toString
    
    Some(
        ParsedKbpSentence(
            kbpSentence.docId, 
            kbpSentence.sentNum.toString, 
            kbpSentence.offset.toString, 
            tokens, 
            postags, 
            chunks, 
            offsets, 
            dgraph))
    } catch {
      case e: Throwable =>
        System.err.println("Error parsing sentence: %s".format(kbpSentence.text))
        e.printStackTrace()
        None
    }
  }
}


object KbpSentenceParser {
  
  import java.util.concurrent.atomic.AtomicInteger
  
  private val sentencesProcessed = new AtomicInteger(0)
  
  def main(args: Array[String]): Unit = {
    
    var webRaw  = Option.empty[String]
    var newsRaw  = Option.empty[String]
    var forumRaw = Option.empty[String] 
    
    var limit = Int.MaxValue
    var outputFile = "stdout"
    var parallel = false
    
    val cliParser = new OptionParser() {
      opt("webFile", "raw web xml", { str => webRaw = Some(str) })
      opt("newsFile", "raw news xml", { str => newsRaw = Some(str) })
      opt("forumFile", "raw forum xml", { str => forumRaw = Some(str) })
      opt("outputFile", "File: ParsedKbpSentences, default stdout", { str => outputFile = str })
      opt("parallel", "Run multithreaded? Default = no", { parallel = true })
      opt("limit", "Limit number of sentences output?", { str => limit = str.toInt })
    }

    val nsTime = Timing.time {
      if (cliParser.parse(args)) {

        val inputCorpora =
          Seq((newsRaw, "news"), (webRaw, "web"), (forumRaw, "forum"))
            .flatMap {
              case (input, corpora) => input match {
                case Some(f) => Some(f, corpora)
                case None => None
              }
            }

        val output = if (outputFile.equals("stdout")) System.out else new PrintStream(outputFile, "UTF8")
        
        runMain(inputCorpora, output, parallel, limit)
      }

    }
    val seconds = Timing.Seconds.format(nsTime)
    System.err.println("Processed %d sentences in %s.".format(sentencesProcessed.get, seconds))
  }

  def runMain(inputCorpora: Seq[(String, String)], output: PrintStream, parallel: Boolean, limit: Int): Unit = {
    
    inputCorpora foreach { case (file, name) => System.err.println("Parsing %s XML from %s".format(name, file)) }
    
    inputCorpora foreach { case (sample, corpus) =>
      val source = Source.fromFile(sample)
      if (parallel) runParallel(source, output, corpus, limit)
      else run(source, output, corpus, limit)
    }
  }
  
  def run(input: Source, output: PrintStream, corpus: String, limit: Int): Unit = {
    
    val docSplitter = new DocSplitter()
    val docParser = KbpDocProcessor.getProcessor(corpus)
    val sentencer = Sentencer.defaultInstance
    val kbpProcessor = new KbpSentenceParser();
    
    val docs = docSplitter.splitDocs(input)
    val parsedDocs = docs flatMap docParser.process
    val sentences = parsedDocs flatMap sentencer.convertToSentences
    
    val parsedKbpSentences = sentences flatMap kbpProcessor.parseKbpSentence
    // take it to the limit (or take until the limit?)
    parsedKbpSentences.take(limit) foreach { parsedKbpSentence =>
      sentencesProcessed.incrementAndGet()
      output.println(ParsedKbpSentence.write(parsedKbpSentence))
    }
  }
  
  val batchSize = 1000
  
  def runParallel(input: Source, output: PrintStream, corpus: String, limit: Int): Unit = {
    
    val docSplitter = new DocSplitter()
    val docParser = KbpDocProcessor.getProcessor(corpus)
    val sentencer = Sentencer.defaultInstance
    val kbpProcessor = new KbpSentenceParser();
    
    val docs = docSplitter.splitDocs(input)
    val parsedDocs = docs flatMap docParser.process
    val sentences = parsedDocs flatMap sentencer.convertToSentences
    
    val parsedSentences = sentences.grouped(batchSize).flatMap { sentenceGroup =>
      sentenceGroup.par.flatMap { sentence =>
        sentencesProcessed.incrementAndGet()
        kbpProcessor.parseKbpSentence(sentence)
      }
    } 
    
    parsedSentences.take(limit) map { parsed => 
      ParsedKbpSentence.write(parsed)  
    } foreach { string =>
      output.println(string)
    }
  }
}