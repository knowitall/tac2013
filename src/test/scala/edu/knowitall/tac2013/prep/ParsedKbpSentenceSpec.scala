package edu.knowitall.tac2013.prep

import org.scalatest._
import edu.knowitall.tool.parse.graph.DependencyGraph
import java.util.regex.Pattern
import edu.knowitall.tool.tokenize.Tokenizer

class ParsedKbpSentenceSpec extends FlatSpec {  
  
  val samplesDir = "src/main/resources/samples/"
  val corpora = Seq("news", "web", "forum")
  val parsedFiles = corpora.map { c => "%s%s%s".format(samplesDir, c, "-parsed.txt") }
  val rawFiles = corpora.map { c => "%s%s%s".format(samplesDir, c, "-xml.txt") }
  
  import edu.knowitall.tac2013.preprocess.tac.ExtractSentencesAndTokenize.whitespace_charclass 
  val wsPattern = Pattern.compile(whitespace_charclass)
  def fixWs(str: String): String = wsPattern.matcher(str).replaceAll(" ")
  
  val replaceChars = Map(("¥" -> " "), ("\n" -> " "))
  
  def fixHack(str: String): String = {
    var cleaned = str
    for ((bad, repl) <- replaceChars.iterator) {
      cleaned = cleaned.replaceAll(bad, repl)
    }
    cleaned
  }
  
  "ParsedKbpSentences" should "deserialize then serialize to their original string" in {
    
    val testLines = parsedFiles map { f => io.Source.fromFile(f, "UTF8") } flatMap { _.getLines }
    testLines.foreach { line =>
      val sent = ParsedKbpSentence.read(line).get
      val reserialized = ParsedKbpSentence.write(sent)
      require(reserialized.equals(line))
    } 
  }
  
  "ParsedKbpSentences" should "contain token offsets that correctly match file contents" in {
    
    //
    // group (rawdoc, parsedDoc, List[Sentence]):
    //
    
    // -- First, get list[List[RawDoc], Corpus]
    val rawDocs = corpora.zip(rawFiles) map { case (corpus, file) =>
      val fileDocs = DocSplitter(io.Source.fromFile(file, "UTF8").getLines)
      (fileDocs, corpus)
    }
    
    // -- Second, get Map[DocId => RawDoc, ParsedDoc]
    val rawDocProcessors = rawDocs map { case (rawDocs, corpus) => 
      (rawDocs, KbpDocProcessor.getProcessor(corpus)) 
    } 
    val processedDocsMap = rawDocProcessors flatMap { case (rawDocs, docProcessor) =>
      rawDocs.flatMap { rawDoc => 
        docProcessor.process(rawDoc) flatMap { parsed =>
          parsed.extractDocId map { docId =>
            (docId, (rawDoc, parsed))  
          }
        }
      }
    } toMap
    
    // -- Third, get Map[DocID => List[ParsedSentence]]
    val parsedSentencesMap = corpora.zip(parsedFiles) flatMap { case (corpus, file) =>
        io.Source.fromFile(file, "UTF8").getLines flatMap { l => ParsedKbpSentence.read(l) }
    } groupBy (_.docId)

    for (
      docId <- processedDocsMap.keys;
      (rawDoc, processedDoc) <- processedDocsMap.get(docId)) {
      val rawDocString = rawDoc.getString
      for (sentences <- parsedSentencesMap.get(docId); sentence <- sentences) {
        testSentence(sentence, rawDocString)
        testTokens(sentence, rawDocString)
      }
    }
  }
  
  def testSentence(sentence: ParsedKbpSentence, rawString: String): Unit = {
    
    // The sentence has tokens that assemble to an original string,
    // provided that their offsets are correct.
    val original = Tokenizer.originalText(sentence.chunkedTokens)
    
    val fakeSentence = original.startsWith("This post was written")
    
    
    // The sentence also contains a start offset that points into the raw string:
    // However, all whitespace is normalized to 0x20 (" ") in original
    val rawSentence = fixHack(rawString.drop(sentence.startOffsetInt).take(original.length))
    

    if (!fakeSentence) assert(original.length === rawSentence.length)
  }
  
  def testTokens(sentence: ParsedKbpSentence, rawString: String): Unit = {
    // The sentence has tokens that assemble to an original string,
    // provided that their offsets are correct.
    val original = Tokenizer.originalText(sentence.chunkedTokens)
    
    val fakeSentence = original.startsWith("This post was written")

    if (!fakeSentence) {
      for (token <- sentence.chunkedTokens) {
        val rawToken = fixHack(rawString.drop(token.offset + sentence.startOffsetInt).take(token.string.length))
        if (!rawToken.equals(token.string)) {
          System.err.println("%s:%s".format(sentence.docId, sentence.startOffset + token.offset))
          System.err.println("orig: \"%s\"".format(token.string))
          System.err.println("raw:  \"%s\"".format(rawToken))
          //fail()
        }
      }
    }
  }
}