package edu.knowitall.tac2013.app.util

import edu.knowitall.tac2013.app.Candidate
import edu.knowitall.tac2013.stanford.annotator.utils.StanfordAnnotatorHelperMethods
import edu.knowitall.tac2013.app.Slot
import edu.stanford.nlp.dcoref.CorefChain.CorefMention
import edu.knowitall.collection.immutable.Interval
import edu.knowitall.tac2013.solr.query.SolrHelper
import scala.collection.JavaConverters._

object DocUtils {
  
  val stanfordHelper = new StanfordAnnotatorHelperMethods()
  
  def putInTimexFormat(slotCandidates: Map[Slot,Seq[Candidate]]): Unit = stanfordHelper.synchronized {
    
    for(slot <- slotCandidates.keys){
      if(slot.isDate){
        //get timex format from SUTime
        val candidates = slotCandidates(slot)
        for(candidate <- candidates){
          val timexFormattedFill = stanfordHelper.getNormalizedDate(candidate.fillOffsetInterval, candidate.extr.sentence.docId ,candidate.trimmedFill.string)
          println(timexFormattedFill + " " + candidate.trimmedFill.string)
          candidate.trimmedFill.setString(timexFormattedFill)
        }
        
      }
    }
    
  }
  
  def getCorefMentions(docId: String, interval: Interval): Option[List[CorefMention]] = stanfordHelper.synchronized {
    val rawDoc = SolrHelper.getRawDoc(docId)
    val mentions = stanfordHelper.getCorefMentions(rawDoc, interval)
    if(mentions.isEmpty()){
      return None
    }
    else {
     return Some(mentions.asScala.toList)
    }
  }
  
  def main(args: Array[String]){
    
    SolrHelper.setConfigurations("old",false)
    
    if(args.length != 3){
      throw new Exception("There must be three args, arg 1 is docId from the old corpus, arg2 is beginning offset of string and arg3 is ending offset of string")
    }
    
    val mentions = getCorefMentions(args(0),Interval.closed(args(1).toInt,args(2).toInt))
    if(mentions.isDefined){
	    for(m <- mentions.get){
	      println(m.mentionSpan)
	    }
    }
    
  }
  
  def findBestFillMention(slotCandidates: Map[Slot,Seq[Candidate]]){
    
    for(slot <-slotCandidates.keys){
      for(candidate <- slotCandidates(slot)){
        
      }
    }
    
    
  }

}