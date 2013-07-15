package edu.knowitall.tac2013.benchmark

import edu.knowitall.tac2013.solr.query.SolrQueryExecutor
import edu.knowitall.tac2013.app.Slot
import java.io.PrintStream

/**
 * okFills contains the set of acceptable answers (e.g. Slotfill and alternates). The primary (Slotfill) will always
 * be the first element...
 */
case class BenchmarkAnswer(val okFills: Seq[String], val docId: String) {
  
}

case class BenchmarkItem(val entityName: String, val entityType: String, val nodeId: String, val slot: Slot, val answers: Set[BenchmarkAnswer]) {
  
  import edu.knowitall.tac2013.app.KBPQuery
  import edu.knowitall.tac2013.app.KBPQueryEntityType
  
  def printString(answer: BenchmarkAnswer) = (Seq(entityName, entityType, nodeId, answer.docId, slot.name) ++ answer.okFills).mkString("\t")
  def printStrings = (answers.toSeq map printString)
  
  
  def kbpQuery = {
    val nodeIdOpt = if (nodeId.startsWith("NIL")) None else Some(nodeId)
    val eType = KBPQueryEntityType.fromString(entityType)
    KBPQuery.forEntityName(entityName, eType, nodeIdOpt).withOverrideSlots(Set(slot))
  }
}

class Benchmarker(val solrExec: SolrQueryExecutor, val benchmarkItems: Iterable[BenchmarkItem], output: PrintStream) {

  import edu.knowitall.tac2013.app.Candidate
  import edu.knowitall.tac2013.app.FilterSolrResults
  import edu.knowitall.tac2013.app.OutputFormatter
  import edu.knowitall.tac2013.app.SlotFillReranker
  import edu.knowitall.tac2013.app.SlotFillConsistency
  import java.io.PrintStream
  
  private val nullOutput = new OutputFormatter(new PrintStream("/dev/null"), printFiltered = false, printUnfiltered = false, printGroups = false, printAnswers = false)
  
  private def getResponse(item: BenchmarkItem): Seq[Candidate] = {
    
    val unfiltered = solrExec.executeUnfilteredQuery(item.kbpQuery, item.slot)

    val filteredCandidates = FilterSolrResults.filterResults(unfiltered, item.kbpQuery)

    val bestAnswers = new SlotFillReranker(nullOutput).findSlotAnswers(item.slot, item.kbpQuery, filteredCandidates)

    val smoothedSlotBestAnswers = SlotFillConsistency.makeConsistent(Map(item.slot -> bestAnswers))
    
    smoothedSlotBestAnswers(item.slot)
  }
  
  private def judgeResponses(responses: Seq[Candidate], item: BenchmarkItem) {
    
    var unusedAnswers = item.answers
    var usedAnswers: Set[BenchmarkAnswer] = Set.empty
    for (response <- responses) {
      // find an answer that matches
      val matchingAnswer = unusedAnswers.find(_.okFills.contains(response.trimmedFill))
      matchingAnswer match {
        case Some(answer) => {
          unusedAnswers -= answer
          usedAnswers += answer
          correct(response)
        }
        case None => {
          notInBenchmark(response)
        }
      }
    }
    
    for (answer <- unusedAnswers) {
      notFound(answer, item)
    }
  }
  
  private val correctPrefix        = "(     CORRECT    )"
  private val notInBenchmarkPrefix = "(Not In Benchmark)"
  private val notFoundPrefix       = "(    Not Found   )"
  
  private def correct(candidate: Candidate): Unit = {
    output.println(s"$correctPrefix ${candidate.debugString}")
  }
  
  private def notInBenchmark(candidate: Candidate): Unit = {
    output.println(s"notInBenchmarkPrefix ${candidate.debugString}")
  }
  
  private def notFound(answer: BenchmarkAnswer, item: BenchmarkItem): Unit = {
    output.println(s"notFoundPrefix ${item.printString(answer)}")
  }
  
  def go: Unit = {
    benchmarkItems foreach { item => judgeResponses(getResponse(item), item) } 
  }
}

object Benchmarker {

  import io.Source
  import java.net.URL
  import edu.knowitall.common.Resource.using
  import scopt.OptionParser
  
  val benchmark2012 = "Benchmark_2012.tsv"
  val benchmark2013 = "Benchmark_2013.tsv"
    
  private def loadRsrc(path: String) = Option(getClass.getResource(path)).getOrElse(throw new RuntimeException(path + " not found"))
  
  private def b2012src = loadRsrc(benchmark2012)
  private def b2013src = loadRsrc(benchmark2013)
 
  private val tabRegex = "\t".r
  
  private def loadBenchmark(url: URL) = using(Source.fromURL(url, "UTF8")) { source =>
    
    // drop header line and filter empty lines.
    val filteredLines = source.getLines.drop(1).filter(_.trim.nonEmpty)
    // split on tabs
    val splitLines = filteredLines.map(tabRegex.split).map(_.toList)
    // group by (name, type, nodeId, slotname)
    val groupNames = splitLines.toSeq.groupBy { fields =>
      fields match {
        case name :: typ :: nodeId :: docId :: slotname :: slotfills => (name :: typ :: nodeId :: slotname :: Nil)
        case _ => throw new RuntimeException("(#1) Malformed benchmark item fields:\n" + fields.mkString("\t"))
      }
    }
    // combine everything to get BenchmarkItems.
    val benchmarkItems = groupNames.map {
      case (name :: typ :: nodeId :: slotname :: Nil, splits) => {
        val answers = splits.map { fields =>
          fields match {
            case (_ :: _ :: _ :: docId :: _ :: slotfill :: alternates) =>
              BenchmarkAnswer(slotfill :: alternates, docId)
            case _ => throw new RuntimeException("(#2) Malformed benchmark item fields:\n" + fields.mkString("\t"))
          }
        }
        BenchmarkItem(name, typ, nodeId, Slot.fromName(slotname), answers.toSet)
      }
      case _ => throw new RuntimeException("(#3) Malformed benchmark item fields.")
    }
    benchmarkItems.toList
  }
  
  private def load2012Benchmark = loadBenchmark(b2012src)
  private def load2013Benchmark = loadBenchmark(b2013src)
  
  def main(args: Array[String]): Unit = {
    
    var corpus = "2013"
      
    val parser = new OptionParser() {
      arg("corpus", "2012 or 2013", { corpus = _ })
    }
    
    if (!parser.parse(args)) return
    require(corpus == "2012" || corpus == "2013", "Corpus must be 2012 or 2013")
    
    corpus match {
      case "2013" => new Benchmarker(SolrQueryExecutor.newCorpus, load2013Benchmark, System.out).go
      case "2012" => new Benchmarker(SolrQueryExecutor.oldCorpus, load2012Benchmark, System.out).go
      case _ => throw new RuntimeException("Corpus must be 2012 or 2013")
    }
  }
}