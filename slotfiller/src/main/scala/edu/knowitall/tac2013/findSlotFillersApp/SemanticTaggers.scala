package edu.knowitall.tac2013.findSlotFillersApp

import edu.knowitall.taggers.tag.TaggerCollection
import edu.knowitall.taggers.Type
import edu.knowitall.tool.chunk.OpenNlpChunker
import edu.knowitall.tool.stem.MorphaStemmer
import edu.knowitall.tool.stem.Lemmatized
import edu.knowitall.tool.chunk.ChunkedToken
import scala.io.Source
import edu.knowitall.tac2013.openie.KbpExtraction

object SemanticTaggers {

  //point taggers to relevant xml files
  private lazy val StanfordNERTagger = loadTagger("/edu/knowitall/tac2013/findSlotFillersApp/StanfordNERTaggers")

  private lazy val EducationalOrganizationTagger = loadTagger("/edu/knowitall/tac2013/findSlotFillersApp/EducationalOrganizationTaggers")

  private lazy val NationalityTagger = loadTagger("/edu/knowitall/tac2013/findSlotFillersApp/NationalityTaggers")

  private lazy val ReligionTagger = loadTagger("/edu/knowitall/tac2013/findSlotFillersApp/ReligionTaggers")

  private lazy val JobTitleTagger = loadTagger("/edu/knowitall/tac2013/findSlotFillersApp/JobTitleTaggers")

  private lazy val DateTagger = loadTagger("/edu/knowitall/tac2013/findSlotFillersApp/DateTaggers")
  
  private lazy val CrimeTagger = loadTagger("/edu/knowitall/tac2013/findSlotFillersApp/CrimeTaggers")
  
  private lazy val IntegerTagger = loadTagger("/edu/knowitall/tac2013/findSlotFillersApp/IntegerTaggers")
  
  private def loadTagger(resourcePath: String) = {
    val url = getClass.getResource(resourcePath)
    require(url != null, "Could not find resource: " + resourcePath)
    TaggerCollection.fromPath(url.getPath())
  }

  private val morpha = new MorphaStemmer()

  def useStandfordNERTagger(chunkedSentence: Seq[ChunkedToken]): List[Type] = {
    var tokens = List[Lemmatized[ChunkedToken]]()
    for (token <- chunkedSentence) {
      val lemma = morpha.lemmatizeToken(token);
      tokens = tokens ::: List(lemma)
    }
    val types = scala.collection.JavaConversions.asScalaIterable(StanfordNERTagger.tag(scala.collection.JavaConversions.asJavaList(tokens)))
    types.toList
  }

  def useJobTitleTagger(chunkedSentence: Seq[ChunkedToken]): List[Type] = {

    var tokens = List[Lemmatized[ChunkedToken]]()
    for (token <- chunkedSentence) {
      val lemma = morpha.lemmatizeToken(token);
      tokens = tokens ::: List(lemma)
    }
    val types = scala.collection.JavaConversions.asScalaIterable(JobTitleTagger.tag(scala.collection.JavaConversions.asJavaList(tokens)))
    types.toList
  }

  def useNationalityTagger(chunkedSentence: Seq[ChunkedToken]): List[Type] = {

    var tokens = List[Lemmatized[ChunkedToken]]()
    for (token <- chunkedSentence) {
      val lemma = morpha.lemmatizeToken(token);
      tokens = tokens ::: List(lemma)
    }
    val types = scala.collection.JavaConversions.asScalaIterable(NationalityTagger.tag(scala.collection.JavaConversions.asJavaList(tokens)))
    types.toList
  }

  def useEducationalOrganizationTagger(chunkedSentence: Seq[ChunkedToken]): List[Type] = {

    var tokens = List[Lemmatized[ChunkedToken]]()
    for (token <- chunkedSentence) {
      val lemma = morpha.lemmatizeToken(token);
      tokens = tokens ::: List(lemma)
    }
    val types = scala.collection.JavaConversions.asScalaIterable(EducationalOrganizationTagger.tag(scala.collection.JavaConversions.asJavaList(tokens)))
    types.toList
  }

  def useReligionTagger(chunkedSentence: Seq[ChunkedToken]): List[Type] = {

    var tokens = List[Lemmatized[ChunkedToken]]()
    for (token <- chunkedSentence) {
      val lemma = morpha.lemmatizeToken(token);
      tokens = tokens ::: List(lemma)
    }
    val types = scala.collection.JavaConversions.asScalaIterable(ReligionTagger.tag(scala.collection.JavaConversions.asJavaList(tokens)))
    types.toList
  }

  def useDateTagger(chunkedSentence: Seq[ChunkedToken]): List[Type] = {

    var tokens = List[Lemmatized[ChunkedToken]]()
    for (token <- chunkedSentence) {
      val lemma = morpha.lemmatizeToken(token);
      tokens = tokens ::: List(lemma)
    }
    val types = scala.collection.JavaConversions.asScalaIterable(DateTagger.tag(scala.collection.JavaConversions.asJavaList(tokens)))
    types.toList
  }
  
  def useCrimeTagger(chunkedSentence: Seq[ChunkedToken]): List[Type] = {

    var tokens = List[Lemmatized[ChunkedToken]]()
    for (token <- chunkedSentence) {
      val lemma = morpha.lemmatizeToken(token);
      tokens = tokens ::: List(lemma)
    }
    val types = scala.collection.JavaConversions.asScalaIterable(CrimeTagger.tag(scala.collection.JavaConversions.asJavaList(tokens)))
    types.toList
  }
  
  def useIntegerTagger(chunkedSentence: Seq[ChunkedToken]): List[Type] = {

    var tokens = List[Lemmatized[ChunkedToken]]()
    for (token <- chunkedSentence) {
      val lemma = morpha.lemmatizeToken(token);
      tokens = tokens ::: List(lemma)
    }
    val types = scala.collection.JavaConversions.asScalaIterable(IntegerTagger.tag(scala.collection.JavaConversions.asJavaList(tokens)))
    types.toList
  }
  
  def getTagTypes(extr: KbpExtraction, pattern: SlotPattern): List[Type] = {
    
    
      val sent = extr.sentence.chunkedTokens
      val slotType = pattern.slotType.getOrElse({ "" })
      val slotLocation = pattern.slotFillIn match {
      case Some("arg1") => extr.arg1.tokenInterval
      case Some("arg2") => extr.arg2.tokenInterval
      case Some("relation") => extr.rel.tokenInterval
      case _ => throw new Exception("slot Location must be arg1, arg2, or relation")
    }
      
      var typeList = List[Type]()
      
      if (slotType == "Organization" || slotType == "Person" || slotType == "Stateorprovince" ||
      slotType == "City" || slotType == "Country") {
        
        val types = SemanticTaggers.useStandfordNERTagger(sent)
        typeList = typeList ::: types

    } else if (slotType == "School") {

      val types = SemanticTaggers.useEducationalOrganizationTagger(sent)
      typeList = typeList ::: types

      
    } else if (slotType == "JobTitle") {

      val types = SemanticTaggers.useJobTitleTagger(sent)
      typeList = typeList ::: types

    } else if (slotType == "Nationality") {

      val types = SemanticTaggers.useNationalityTagger(sent)
      typeList = typeList ::: types

    } else if (slotType == "Religion") {

      val types = SemanticTaggers.useReligionTagger(sent)
      typeList = typeList ::: types

    } else if (slotType == "Date") {
      val types = SemanticTaggers.useDateTagger(sent)
      typeList = typeList ::: types

    } else if (slotType == "ProperNoun"){
      //need to figure out what to do for general ProperNoun semantic filter

    }  else if ((slotType =="<integer>-year-old") || (slotType == "Integer")){
      val types = SemanticTaggers.useIntegerTagger(sent)
      typeList = typeList ::: types
      
    }
      
    typeList
  }


}