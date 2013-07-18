package edu.knowitall.tac2013.app

import edu.knowitall.tac2013.openie.KbpExtraction
import edu.knowitall.tac2013.app.LocationHelper.findLocationTaggedType
import edu.knowitall.tac2013.solr.query.SolrQueryType._
import edu.knowitall.tac2013.solr.query.SolrQueryType
import edu.knowitall.collection.immutable.Interval
import scala.util.matching.Regex

object FilterSolrResults {
  
  lazy val semanticCategoryPattern = new Regex("[A-Z<>]\\w+")

  
  private def satisfiesRequirementAtInterval(start: Integer, end: Integer, specificationStrings: Array[String], candidate: Candidate ): Boolean = {
    
        var intervalHead = start
        for(specificationString <- specificationStrings){
          //if the string is a semantic category requirement string
          if(intervalHead >= end){
            return false
          }
          if (semanticCategoryPattern.findFirstIn(specificationString).isDefined){
            val newIntervalHead = updateIntervalHead(specificationString,candidate,Interval.closed(intervalHead,end),false)
            if(newIntervalHead.isDefined){
              intervalHead =  newIntervalHead.get.end
            }
            else{
              return false
            }
          }
          //if the string is simply a word to match
          else{
            val extractionTokenWithoutBrackets = candidate.extr.sentence.chunkedTokens(Interval.closed(intervalHead, intervalHead+1)).head.string.replace("[", "").replace("]", "").toLowerCase()
   
            if(specificationString.toLowerCase().trim() == extractionTokenWithoutBrackets){
              intervalHead = intervalHead+1
            }
            else{
              return false
            }
          }
        }
        return true
  }
  
  
  private def filterOutBrackets(specificationStrings: Array[String], rel: String): Option[Array[String]] = {
    
    var newLength = specificationStrings.length
    val relWords = rel.split(" ")
    if(relWords.last.contains("[")){
      if(relWords.last.replace("[", "").replace("]", "").trim().toLowerCase() == specificationStrings.last){
        return Some(specificationStrings.take(specificationStrings.length-1))
      }
      else{
        return None
      }
    }
    else{
      Some(specificationStrings)
    }
    
  }
  private def satisfiesRequirementAtIntervalFromEnd(start: Integer, end: Integer, specificationStrings: Array[String], candidate: Candidate ): Boolean = {
        val remainingSpecificationStringsOption = filterOutBrackets(specificationStrings,candidate.extr.rel.originalText)
        if(remainingSpecificationStringsOption.isEmpty) return false
        var intervalTail = end
        for(specificationString <- remainingSpecificationStringsOption.get.reverse){
          //if the string is a semantic category requirement string
          if(start >= intervalTail){
            return false
          }
          if (semanticCategoryPattern.findFirstIn(specificationString).isDefined){
            val newIntervalTail = updateIntervalHead(specificationString,candidate,Interval.open(start,intervalTail),true)
            if(newIntervalTail.isDefined){
              intervalTail =  newIntervalTail.get.start
            }
            else{
              return false
            }
          }
          //if the string is simply a word to match
          else{
            val extractionTokenWithoutBrackets = candidate.extr.sentence.chunkedTokens(Interval.closed(intervalTail-1, intervalTail)).head.string.replace("[", "").replace("]", "").toLowerCase()
   
            if(specificationString.toLowerCase().trim() == extractionTokenWithoutBrackets){
              intervalTail = intervalTail -1 
            }
            else{
              return false
            }
          }
        }
        return true
  }
  
  private def intervalMatches(t: Interval, target: Interval, backwards: Boolean): Boolean = {
     
    if(backwards){
      if(t.end == target.end){
        return true
      }
      else{
        return false
      }
      
    }
    else{
      if(t.start == target.start){
        return true
      }
      else{ return false}
    }
  }
  
  private def updateIntervalHead(semanticTypeRaw: String, candidate: Candidate, interval :Interval, backwards: Boolean ): Option[Interval] = {

    val semanticType = semanticTypeRaw.replace("<","").replace(">", "").trim()
    
    val chunkedSentence = candidate.extr.sentence.chunkedTokens
   
    
    if (semanticType == "Organization") {
      val types = SemanticTaggers.useStandfordNERTagger(chunkedSentence)
      for (t <- types) {
         if (t.descriptor() == "StanfordORGANIZATION") {
             if(intervalMatches(t.interval,interval,backwards)){
                return Some(t.interval())
             }
            }
      }
      return None
    } else if(semanticType == "Person"){
      val types = SemanticTaggers.useStandfordNERTagger(chunkedSentence)
      for (t <- types) {
        if(intervalMatches(t.interval,interval,backwards)) {
            if (t.descriptor() == "StanfordPERSON") {
                return Some(t.interval())
            }
        }
      }
      return None
    }
     else if(semanticType == "Location" || semanticType =="City" || semanticType =="Country" || semanticType =="Stateorprovince"){
      val types = SemanticTaggers.useStandfordNERTagger(chunkedSentence)
      for (t <- types) {
        if(intervalMatches(t.interval,interval,backwards)) {
            if (t.descriptor() == "StanfordLOCATION") {
                return Some(t.interval())
            }
        }
      }
      return None
      
    }

    else if (semanticType == "School") {
      val types = SemanticTaggers.useEducationalOrganizationTagger(chunkedSentence)
      for (t <- types) {
        if(intervalMatches(t.interval,interval,backwards)) return Some(t.interval())
      }

      return None

    } else if (semanticType == "JobTitle") {
      val types = SemanticTaggers.useJobTitleTagger(chunkedSentence)
      for (t <- types) {
        if(intervalMatches(t.interval,interval,backwards)) return Some(t.interval())
      }

      return None

    } else if (semanticType == "HeadJobTitle"){
      
      val types = SemanticTaggers.useHeadJobTitleTagger(chunkedSentence)
      for(t <- types){
        if (intervalMatches(t.interval,interval,backwards)) return Some(t.interval())
      }
      
      return None
    }
      else if (semanticType == "Nationality") {

      val types = SemanticTaggers.useNationalityTagger(chunkedSentence)
      for (t <- types) {
        if(intervalMatches(t.interval,interval,backwards)) return Some(t.interval())

      }

      return None

    } else if (semanticType == "Religion") {
      val types = SemanticTaggers.useReligionTagger(chunkedSentence)

      for (t <- types) {
        if(intervalMatches(t.interval,interval,backwards)) return Some(t.interval())
      }

      return None

    } else if (semanticType == "Date") {
      val types = SemanticTaggers.useDateTagger(chunkedSentence)
      
      for (t <- types) {
        if(intervalMatches(t.interval,interval,backwards)) return Some(t.interval())

      }

      return None

    } else if (semanticType == "ProperNoun"){
      val types = SemanticTaggers.useStandfordNERTagger(chunkedSentence)
      for (t <- types) {
        if(intervalMatches(t.interval,interval,backwards)) return Some(t.interval())

      }
      
      return None
    }  else if ((semanticType =="<integer>-year-old") || (semanticType == "Integer")){

      val types = SemanticTaggers.useIntegerTagger(chunkedSentence)
      for (t <- types) {
        if(intervalMatches(t.interval,interval,backwards)) return Some(t.interval())

      }

      return None
      
    } else {
    
    

      return None

    }

    
  }

  //filter for arg2 beginning with proper preposition
  private def satisfiesArg2BeginsFilter(candidate: Candidate): Boolean = {
    

    candidate.pattern.arg2Begins match {
      case Some(arg2BeginsString) => {
        
        val patternSpecificationStrings = arg2BeginsString.split(" ")
        val intervalHead = candidate.extr.arg2.tokenInterval.start
        val end = candidate.extr.arg2.tokenInterval.end
        satisfiesRequirementAtInterval(intervalHead, end, patternSpecificationStrings, candidate)
        
        
      }
      case None => {
        //if nothing specified then throw out any extractions where arg2 does begin with a preposition
        val firstTokenInArg2Option = candidate.extr.arg2.tokens.headOption
        if(firstTokenInArg2Option.isDefined){
          val firstTokenInArg2 = firstTokenInArg2Option.get
          if(firstTokenInArg2.isPreposition){
            false
          }
          else{
            true
          }
        }
        else{
          false
        }
      }
    }
  }

  private def satisfiesRelFilter(candidate: Candidate): Boolean = {

    
    //if there are no semantic strings in the relation condidtion then count backwards for
    //the start interval and call satisfiesRequirementAtInterval
    
    

    candidate.pattern.relString match {
      case Some(relString) => {
        
        val patternSpecificationStrings = relString.split(" ")
        val intervalHead = candidate.extr.rel.tokenInterval.start
        val end = candidate.extr.rel.tokenInterval.end
        val b = satisfiesRequirementAtIntervalFromEnd(intervalHead, end, patternSpecificationStrings, candidate)
        b
      }
      case None => true
    }
  }
  
  private def satisfiesTermFilters(candidate: Candidate) : Boolean = {
    
    
    //arg1 Terms
    val arg1TermsSatisfied =
      candidate.pattern.arg1Terms match {
        case Some(arg1Terms) => {
              val arg1Interval = candidate.extr.arg1.tokenInterval
              val patternSpecificationStrings = arg1Terms.split(" ")
              var foundMatchingSubsequence = false
              for(x  <- arg1Interval){
            	  if(satisfiesRequirementAtInterval(x,arg1Interval.end,patternSpecificationStrings,candidate)){
            	    foundMatchingSubsequence = true
            	  }
              }
              foundMatchingSubsequence
        }
        case None => true
      }


    //arg2 Terms
    val arg2TermsSatisfied =
      candidate.pattern.arg2Terms match {
        case Some(arg2Terms) => {
              val arg2Interval = candidate.extr.arg2.tokenInterval
              val patternSpecificationStrings = arg2Terms.split(" ")
              var foundMatchingSubsequence = false
              for(x  <- arg2Interval){
            	  if(satisfiesRequirementAtInterval(x,arg2Interval.end,patternSpecificationStrings,candidate)){
            	    foundMatchingSubsequence = true
            	  }
              }
              foundMatchingSubsequence
        }
        case None => true
      }
    
    
    (arg1TermsSatisfied && arg2TermsSatisfied)
  }

  private def satisfiesEntityFilter(kbpQuery: KBPQuery)(candidate: Candidate): Boolean = {

    val pattern = candidate.pattern
    val queryEntity = kbpQuery.name
    
    if (candidate.queryType == SolrQueryType.REGULAR) {
      if (pattern.isValid) {

        val entityIn = pattern.entityIn.get.trim()
        val entityFromExtraction = entityIn.toLowerCase() match {
          case "arg1" => candidate.extr.arg1.originalText
          case "arg2" => candidate.extr.arg2.originalText
          case _ => throw new Exception("Poorly formatted entityIn field, should be arg1 or arg2")
        }

        val entityFromExtractionSplit = entityFromExtraction.toString().split(" ")

        val queryEntityReversedSplit = queryEntity.trim().split(" ").reverse

        var count = 0
        for (term <- queryEntityReversedSplit) {
          val extractionWord = entityFromExtractionSplit(entityFromExtractionSplit.length - 1 - count)
          if (term.toLowerCase() != extractionWord.toLowerCase()) {
            //(term.toLowerCase() != extractionWord.substring(1,extractionWord.length-1).toLowerCase())){
            return false
          }

          count = count + 1
        }

        true
      } else {

        throw new Exception("KbpSlotToOpenIEData instance is not valid.")
        false
      }
    }
    else if (candidate.queryType == SolrQueryType.COREF){
      //filter out if the entity slot contains a wikiLinkNodeID
      candidate.pattern.entityIn.getOrElse({""}) match {
        case "arg1" => { if(candidate.extr.arg1.wikiLink.isDefined) return false}
        case "arg2" => { if(candidate.extr.arg2.wikiLink.isDefined) return false}
      }
      
      //filter out if there is no mention of the target entity in the current sentence or the preceeding two sentences
      val sentencesWhereEntityIsMentioned = kbpQuery.docIdToSentNumDocIdPairMap(candidate.extr.sentence.docId).map(x => x._2)
      val thisSentenceNum = candidate.extr.sentence.sentNum
      val sentenceRange = thisSentenceNum-2 to thisSentenceNum
      for(sentenceNum <- sentencesWhereEntityIsMentioned){
        if(sentenceRange.contains(sentenceNum)){
          return true
        }
      }
      
      //if there is no sentence with a mention of the entity in the specified range
      //around the coref extraction then the candidate extraction should be filtered out.
      false
    }
    
    else {
      true
    }
  }
  

  private def satisfiesSemanticFilter(candidate: Candidate): Boolean = {

    val pattern = candidate.pattern
    val types = candidate.types
    val trimmedFill = candidate.trimmedFill
    val slotType = pattern.slotType.getOrElse({ "" })
    val slotLocation = pattern.slotFillIn match {
      case Some("arg1") => candidate.extr.arg1.tokenInterval
      case Some("arg2") => candidate.extr.arg2.tokenInterval
      case Some("relation") => candidate.extr.rel.tokenInterval
      case _ => return false
    }

    val chunkedSentence = candidate.extr.sentence.chunkedTokens

    if (slotType == "Organization" || slotType == "Person" || slotType == "Stateorprovince" ||
      slotType == "City" || slotType == "Country") {

      for (t <- types) {
        if (t.interval().intersects(slotLocation)) {
          if(t.interval().start - slotLocation.start < 5){
	          slotType match {
	            case "Organization" => {
	              if (t.descriptor() == "StanfordORGANIZATION") {
	
	                return true
	              }
	            }
	            case "Person" => {
	              if (t.descriptor() == "StanfordPERSON") {
	                return true
	              }
	            }
	            // default case will be location
	            case _ => {
	              //if trimmed Fill does not exist then the Candidate
	              //constructor has filtered out this extraction
	              val typesInSlotFill = types.filter(t => (t.interval().intersects(slotLocation)))
	              if(findLocationTaggedType(typesInSlotFill,slotType).isDefined){
	                return true
	              }
	              else{
	                return false
	              }
	
	            }
	          }
          }
        }
      }

      return false
    } else if (slotType == "School" || slotType == "JobTitle" ||slotType == "HeadJobTitle" ||
        slotType == "Nationality" || slotType == "Religion" || slotType == "Date" ||
        slotType == "ProperNoun" || slotType =="<integer>-year-old" || slotType == "Integer") {
      
      for (t <- types) {
        if (t.interval().intersects(slotLocation)){
          if(t.interval().start - slotLocation.start < 5){
            return true
          }
        } 

      }

      return false
    }

    else {
    
    

      return true

    }

  }
  
  def satisfiesLengthFilter(candidate: Candidate): Boolean = {
    
    //if an alternate name slot fill is longer than 5 tokens, it should be filtered out
    if(Slot.fromName(candidate.pattern.slotName).isAlternateName){
      if(candidate.trimmedFill.interval.length >5) return false
    }
    
    
    true
  }

  //filters results from solr by calling helper methods that look at the KbpSlotToOpenIEData specifications and compare
  //that data with the results from solr to see if the relation is still a candidate
  //
  def filterResults(unfiltered: Seq[Candidate], kbpQuery: KBPQuery): Seq[Candidate] = {
    
    
    def combinedFilter(candidate: Candidate) = (
            satisfiesArg2BeginsFilter(candidate) &&
            satisfiesRelFilter(candidate) &&
            satisfiesTermFilters(candidate) &&
            satisfiesEntityFilter(kbpQuery)(candidate) &&
            satisfiesSemanticFilter(candidate) &&
            satisfiesLengthFilter(candidate))
    
    unfiltered filter combinedFilter
  }
}