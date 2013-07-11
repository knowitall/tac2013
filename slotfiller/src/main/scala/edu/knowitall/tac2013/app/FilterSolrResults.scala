package edu.knowitall.tac2013.app

import edu.knowitall.tac2013.openie.KbpExtraction
import edu.knowitall.tac2013.app.LocationHelper.findLocationTaggedType
import edu.knowitall.tac2013.solr.query.SolrQueryType._
import edu.knowitall.tac2013.solr.query.SolrQueryType

object FilterSolrResults {

  //filter for arg2 beginning with proper preposition
  private def satisfiesArg2PrepositionFilter(candidate: Candidate): Boolean = {

    candidate.pattern.arg2Begins match {
      case Some(arg2PrepositionString) => {
        val solrResultsArg2 = candidate.extr.arg2.originalText
        (solrResultsArg2.toString().toLowerCase().substring(0, arg2PrepositionString.length) == arg2PrepositionString.toLowerCase())
      }
      case None => true
    }
  }

  private def satisfiesRelFilter(candidate: Candidate): Boolean = {

    val pattern = candidate.pattern
    
    if (pattern.isValid) {

      if (!pattern.relString.get.contains("<JobTitle>")) {

        val relationTerms = pattern.relString.get.trim().split(" ")

        val relationTermsReversed = relationTerms.reverse

        val relationTermsFromExtraction = candidate.extr.rel.originalText.trim().split(" ")

        var count = 0
        for (term <- relationTermsReversed) {
          val sentenceWord = relationTermsFromExtraction(relationTermsFromExtraction.length - 1 - count)
          if ((term.toLowerCase() != sentenceWord.toLowerCase()) &&
            (term.replace("[", "").replace("]", "").toLowerCase() != sentenceWord.replace("[", "").replace("]", "").toLowerCase())) {
            return false
          }
          count = count + 1
        }
        return true
      } else {
        val chunkedSentence = candidate.extr.sentence.chunkedTokens
        val types = SemanticTaggers.useJobTitleTagger(chunkedSentence)
        val relLocation = candidate.extr.rel.tokenInterval

        for (t <- types) {
          if (t.interval().intersects(relLocation)) {
            return true
          }
        }
        return false
      }
    } else {

      throw new Exception("KbpSlotToOpenIEData instance is not valid.")
      false
    }
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

      return false
    } else if (slotType == "School") {
      
      for (t <- types) {
        if (t.interval().intersects(slotLocation)) return true

      }

      return false

    } else if (slotType == "JobTitle") {


      for (t <- types) {
        if (t.interval().intersects(slotLocation)) return true

      }

      return false

    } else if (slotType == "Nationality") {


      for (t <- types) {
        if (t.interval().intersects(slotLocation)) return true

      }

      return false

    } else if (slotType == "Religion") {



      for (t <- types) {
        if (t.interval().intersects(slotLocation)) return true

      }

      return false

    } else if (slotType == "Date") {

      for (t <- types) {
        if (t.interval().intersects(slotLocation)) return true

      }

      return false

    } else if (slotType == "ProperNoun"){
      //need to figure out what to do for general ProperNoun semantic filter
      
      return true
    }  else if ((slotType =="<integer>-year-old") || (slotType == "Integer")){

      for (t <- types) {
        if (t.interval().intersects(slotLocation)) return true

      }

      return false
      
    } else {
    
    

      return true

    }

  }

  //filters results from solr by calling helper methods that look at the KbpSlotToOpenIEData specifications and compare
  //that data with the results from solr to see if the relation is still a candidate
  //
  def filterResults(unfiltered: Seq[Candidate], kbpQuery: KBPQuery): Seq[Candidate] = {
    
    def combinedFilter(candidate: Candidate) = (
            satisfiesArg2PrepositionFilter(candidate) &&
            satisfiesEntityFilter(kbpQuery)(candidate) &&
            satisfiesRelFilter(candidate) &&
            satisfiesSemanticFilter(candidate))
    
    unfiltered filter combinedFilter
  }
}