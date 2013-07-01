package edu.knowitall.tac2013.findSlotFillersApp

import edu.knowitall.taggers.tag.TaggerCollection
import edu.knowitall.taggers.Type
import edu.knowitall.tool.chunk.OpenNlpChunker
import edu.knowitall.tool.stem.MorphaStemmer
import edu.knowitall.tool.stem.Lemmatized
import edu.knowitall.tool.chunk.ChunkedToken
import scala.io.Source

object SemanticTaggers {
  
  //point taggers to relevant xml files
  private val StanfordNERTagger = {
    val resourcePath = "/edu/knowitall/tac2013/findSlotFillersApp/StanfordNERTaggers"
    val url = getClass.getResource(resourcePath)
    require(url != null, "Could not find resource: " + resourcePath)
    TaggerCollection.fromPath(url.getPath())
  }
  
  private val EducationalOrganizationTagger = {
    val resourcePath = "/edu/knowitall/tac2013/findSlotFillersApp/EducationalOrganizationTaggers"
    val url = getClass.getResource(resourcePath)
    require(url != null, "Could not find resource: " + resourcePath)
    TaggerCollection.fromPath(url.getPath())
  }
  
  private val NationalityTagger = {
    val resourcePath = "/edu/knowitall/tac2013/findSlotFillersApp/NationalityTaggers"
    val url = getClass.getResource(resourcePath)
    require(url != null, "Could not find resource: " + resourcePath)
    TaggerCollection.fromPath(url.getPath())
  }
  
  private val ReligionTagger = {
    val resourcePath = "/edu/knowitall/tac2013/findSlotFillersApp/ReligionTaggers"
    val url = getClass.getResource(resourcePath)
    require(url != null, "Could not find resource: " + resourcePath)
    TaggerCollection.fromPath(url.getPath())
  }
  
  private val JobTitleTagger = {
    val resourcePath = "/edu/knowitall/tac2013/findSlotFillersApp/JobTitleTaggers"
    val url = getClass.getResource(resourcePath)
    require(url != null, "Could not find resource: " + resourcePath)
    TaggerCollection.fromPath(url.getPath())
  }
  
  private val DateTagger = {
    val resourcePath = "/edu/knowitall/tac2013/findSlotFillersApp/DateTaggers"
    val url = getClass.getResource(resourcePath)
    require(url != null, "Could not find resource: " + resourcePath)
    TaggerCollection.fromPath(url.getPath())
  }
  
  def useStandfordNERTagger(sentence: String): List[Type] = {
      val chunker = new OpenNlpChunker();
	  val morpha = new MorphaStemmer();
	  
	  val chunkedSentence = chunker.chunk(sentence)
	  var tokens = List[Lemmatized[ChunkedToken]]()
	  for (token <- chunkedSentence) {
	        val lemma = morpha.lemmatizeToken(token);
	        tokens = tokens ::: List(lemma)
	  }
	  val types = scala.collection.JavaConversions.asScalaIterable(StanfordNERTagger.tag(scala.collection.JavaConversions.asJavaList(tokens)))
	  types.toList
  }
  
  def useJobTitleTagger(sentence: String): List[Type] = {
      val chunker = new OpenNlpChunker();
	  val morpha = new MorphaStemmer();
	  
	  val chunkedSentence = chunker.chunk(sentence)
	  var tokens = List[Lemmatized[ChunkedToken]]()
	  for (token <- chunkedSentence) {
	        val lemma = morpha.lemmatizeToken(token);
	        tokens = tokens ::: List(lemma)
	  }
	  val types = scala.collection.JavaConversions.asScalaIterable(JobTitleTagger.tag(scala.collection.JavaConversions.asJavaList(tokens)))
	  types.toList
  }
  
  def useNationalityTagger(sentence: String): List[Type] = {
      val chunker = new OpenNlpChunker();
	  val morpha = new MorphaStemmer();
	  
	  val chunkedSentence = chunker.chunk(sentence)
	  var tokens = List[Lemmatized[ChunkedToken]]()
	  for (token <- chunkedSentence) {
	        val lemma = morpha.lemmatizeToken(token);
	        tokens = tokens ::: List(lemma)
	  }
	  val types = scala.collection.JavaConversions.asScalaIterable(NationalityTagger.tag(scala.collection.JavaConversions.asJavaList(tokens)))
	  types.toList
  }
  
  def useEducationalOrganizationTagger(sentence: String): List[Type] = {
      val chunker = new OpenNlpChunker();
	  val morpha = new MorphaStemmer();
	  
	  val chunkedSentence = chunker.chunk(sentence)
	  var tokens = List[Lemmatized[ChunkedToken]]()
	  for (token <- chunkedSentence) {
	        val lemma = morpha.lemmatizeToken(token);
	        tokens = tokens ::: List(lemma)
	  }
	  val types = scala.collection.JavaConversions.asScalaIterable(EducationalOrganizationTagger.tag(scala.collection.JavaConversions.asJavaList(tokens)))
	  types.toList
  }
  
  def useReligionTagger(sentence: String): List[Type] = {
      val chunker = new OpenNlpChunker();
	  val morpha = new MorphaStemmer();
	  
	  val chunkedSentence = chunker.chunk(sentence)
	  var tokens = List[Lemmatized[ChunkedToken]]()
	  for (token <- chunkedSentence) {
	        val lemma = morpha.lemmatizeToken(token);
	        tokens = tokens ::: List(lemma)
	  }
	  val types = scala.collection.JavaConversions.asScalaIterable(ReligionTagger.tag(scala.collection.JavaConversions.asJavaList(tokens)))
	  types.toList
  }
  
  def useDateTagger(sentence: String): List[Type] = {
      val chunker = new OpenNlpChunker();
	  val morpha = new MorphaStemmer();
	  
	  val chunkedSentence = chunker.chunk(sentence)
	  var tokens = List[Lemmatized[ChunkedToken]]()
	  for (token <- chunkedSentence) {
	        val lemma = morpha.lemmatizeToken(token);
	        tokens = tokens ::: List(lemma)
	  }
	  val types = scala.collection.JavaConversions.asScalaIterable(DateTagger.tag(scala.collection.JavaConversions.asJavaList(tokens)))
	  types.toList
  }
  

}