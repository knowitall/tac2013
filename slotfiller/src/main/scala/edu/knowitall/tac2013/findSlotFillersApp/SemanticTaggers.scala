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
}