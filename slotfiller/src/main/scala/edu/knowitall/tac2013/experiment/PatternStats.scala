package edu.knowitall.tac2013.experiment

import edu.knowitall.tac2013.findSlotFillersApp.SlotPattern
import edu.knowitall.tac2013.findSlotFillersApp.Slot
import edu.knowitall.tac2013.solr.query.SolrQueryBuilder
import edu.knowitall.tac2013.findSlotFillersApp.KBPQuery
import edu.knowitall.tac2013.solr.populate.SolrSimpleExecutor

/**
 * Runs test queries for all of the per:/org: patterns and reports statistics about
 * how many results match for a given entity, or for a wildcard entity.
 */
class PatternStats(val solrExec: SolrSimpleExecutor) {
  def this(url: String) = this(new SolrSimpleExecutor(url, 1000))

  val sampleOrgs = Set("Carnival Cruise", "Microsoft", "Apple", "Washington Post", "Discovery Channel", "Stanford University")
  val samplePers = Set("Bill Gates", "Steve Jobs", "Ronnie Sinclair", "Barack Obama", "Steve Ballmer", "Abraham Lincoln", "George Washington", "Babe Ruth")
  
  def run() {

    val perPatternStats = Slot.personSlots.toSeq.map { perSlot =>
        patternStats(perSlot.name, perSlot.patterns, samplePers)
    }

    reportResults(perPatternStats)

    println()
    println()
        
    val orgPatternStats = Slot.orgSlots.toSeq.map { perSlot =>
        patternStats(perSlot.name, perSlot.patterns, samplePers)
    }

    
    reportResults(orgPatternStats)    
  }
  
  def reportResults(stats: Seq[(String, Double, Double)]): Unit = {
    stats foreach {
      case (slotname, avgWcResults, normTotalResults) =>
        println(Seq(padToEllipses(slotname, 50), padToEllipses(avgWcResults.toString, 15), "%.02f" format normTotalResults).mkString("\t"))
    }
    val avgAllWc = stats.map(_._2).sum / stats.size
    val avgAllSamp = stats.map(_._3).sum / stats.size
    println(Seq(padToEllipses("OVERALL AVERAGE", 75), padToEllipses(avgAllWc.toString, 15), "%.02f" format avgAllSamp).mkString("\t"))
  }

  /** Return Slotname, avg wildcard results, avg normal results */
  def patternStats(slotname: String, patterns: Seq[SlotPattern], sampleEntities: Set[String]): (String, Double, Double) = {
    // map patterns to wildcard queries
    val wildcardQueries = patterns.filter(_.isValid).map { p => getQueryString("*", p) }

    // map patterns to sample query sets (for stats)
    val sampleQuerySets = patterns.filter(_.isValid).map { p =>
      sampleEntities.map { e => getQueryString(e, p) }
    }

    // build list of (wildcard query string, total wildcard results, total results over samples)
    val patternStats = wildcardQueries.zip(sampleQuerySets).map {
      case (wc, samples) =>
        val numWildcardResults = solrExec.query(wc).numResults
        val avgSampleResults = samples.map(s => solrExec.query(s).numResults).sum / samples.size.toDouble
        (wc, numWildcardResults, avgSampleResults)
    }

    (slotname, patternStats.map(_._2).sum, patternStats.map(_._3).sum)
  }

  def getQueryString(entity: String, pattern: SlotPattern): String = {
    val kbpQuery = KBPQuery.forEntityName(entity, pattern.entityType, None)
    val qb = new SolrQueryBuilder(pattern, kbpQuery)
    qb.regularQuery.get.queryString
  }

  def padToEllipses(str: String, len: Int): String = {
    if (str.length > len - 3) {
      str.take(len - 3) + "..."
    } else {
      str.padTo(len, " ").mkString
    }
  }
}

object PatternStats {

  def main(args: Array[String]): Unit = {

    new PatternStats(args(0)).run()
  }
}