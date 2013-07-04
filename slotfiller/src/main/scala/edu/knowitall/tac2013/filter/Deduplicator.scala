package edu.knowitall.tac2013.filter

import edu.knowitall.tac2013.openie._
import edu.knowitall.tool.chunk.ChunkedToken
import edu.knowitall.tac2013.openie.solr
import scopt.OptionParser
import edu.knowitall.tac2013.openie.solr.SolrSimpleExecutor

/**
 * Primary interface:
 * input: Seq[KbpExtraction] (possibly with many duplicates)
 * output: Seq[KbpExtraction]
 */
object Deduplicator {

  /**
   * Defines a function that maps "candidate duplicates"
   * to the same key.
   */
  private def extractionKey(extr: KbpExtraction): String = {
    
    def tokenKey(tokens: TraversableOnce[ChunkedToken]): String = {
      tokens.filter(tok => tok.isNoun || tok.isPronoun || tok.isVerb).map(_.string).mkString(" ")
    }
    
    def argKey(arg: KbpArgument) = arg.wikiLink match {
      case Some(WikiLink(name, fbid, nodeIdOpt)) => fbid
      case None => tokenKey(arg.tokens)
    }

    val arg1Key = argKey(extr.arg1)
    val relKey  = tokenKey(extr.rel.tokens)
    val arg2Key = argKey(extr.arg2)
    
    Seq(arg1Key, relKey, arg2Key).mkString(", ")
  }
  
  private def sentenceKey(extr: KbpExtraction): String = {
    extr.sentence.dgraph.text
  } 

  def apply(extrs: Seq[KbpExtraction]): Seq[KbpExtraction] = {
    
    dedupeExtrs(extrs).filter { case (keep, _) => keep } map { case (_, extr) => extr }
  }
  
  private def dedupeExtrs(extrs: Seq[KbpExtraction]): Seq[(Boolean, KbpExtraction)] = {
    // first, group candidate duplicates together.
    val groups = extrs groupBy extractionKey
    
    // for each group, deduplicate sentences
    groups.values.toSeq flatMap dedupeGroup
  }
  
  private def dedupeGroup(extrs: Seq[KbpExtraction]): Seq[(Boolean, KbpExtraction)] = {

    // keep only one of each sentence
    val groupedBySentence = extrs groupBy sentenceKey
    
    groupedBySentence.iterator flatMap { case (sentenceKey, extrs) => 
      val descConf = extrs.sortBy(-_.confidence).toList
      (true, descConf.head) :: descConf.tail.map(e => (false, e))
    } toSeq
  }
  
  def main(args: Array[String]): Unit = {
    
    var solrUrl = ""
    var queryString = ""
      
    val parser = new OptionParser() {
      arg("solrUrl", "Solr URL.", { solrUrl = _ })
      arg("query", "Solr Query String.", { queryString = _})
    }  
    
    if (!parser.parse(args)) return
    
    val solrExec = new SolrSimpleExecutor(solrUrl, 10000)
    
    val extrs = solrExec.query(queryString).results
    
    val judgements = dedupeExtrs(extrs)
    
    judgements foreach { case (keep, extr) => 
      val extrKey = extractionKey(extr).take(25).padTo(25, " ").mkString
      if (keep) println()
      val fields = Seq(extrKey, if (keep) "true " else "false", debugString(extr))
      println(fields.mkString("\t"))
    }
  }

  private def debugString(extr: KbpExtraction): String = {
    "arg1: " + extr.arg1.originalText + "\t rel: " + extr.rel.originalText +
      "\t arg2: " + extr.arg2.originalText + "\t docID: " + extr.sentence.docId +
      "\t confidence: " + extr.confidence + "\t sentence: " + extr.sentence.dgraph.text
  }
}