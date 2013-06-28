package edu.knowitall.tac2013.preprocess.coref;

import java.util.List;
import java.util.Properties;

import edu.stanford.nlp.dcoref.CorefCoreAnnotations.CorefChainAnnotation;
import edu.stanford.nlp.ling.CoreAnnotations.NamedEntityTagAnnotation;
import edu.stanford.nlp.ling.CoreAnnotations.PartOfSpeechAnnotation;
import edu.stanford.nlp.ling.CoreAnnotations.SentencesAnnotation;
import edu.stanford.nlp.ling.CoreAnnotations.TextAnnotation;
import edu.stanford.nlp.ling.CoreAnnotations.TokensAnnotation;
import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.pipeline.Annotation;
import edu.stanford.nlp.pipeline.StanfordCoreNLP;
import edu.stanford.nlp.trees.TreeCoreAnnotations.TreeAnnotation;
import edu.stanford.nlp.trees.semgraph.SemanticGraphCoreAnnotations.CollapsedCCProcessedDependenciesAnnotation;
import edu.stanford.nlp.util.CoreMap;

public class TestCoref {

	public static void main(String[] args) throws Exception {
		   // creates a StanfordCoreNLP object, with POS tagging, lemmatization, NER, parsing, and coreference resolution
	    Properties props = new Properties();
	    props.put("annotators", "tokenize, ssplit, pos, lemma, ner, parse, dcoref");
	    //props.put("annotators", "tokenize,ssplit");
	    StanfordCoreNLP pipeline = new StanfordCoreNLP(props);

	    // read some text in the text variable
	    //String text = "Obama, president of the United States, gave a speech (Reuters)."; // Add your text here!
	    //String text = "To account for gravity's feebleness, Randall and Sundrum borrow some ideas from string theory but add their own twist.";
	    String text= " What if, they ask, higher dimensions are not small and curled up but large, perhaps infinite in size?";

	    // create an empty Annotation just with the given text
	    Annotation document = new Annotation(text);

	    // run all Annotators on this text
	    pipeline.annotate(document);

	    // these are all the sentences in this document
	    // a CoreMap is essentially a Map that uses class objects as keys and has values with custom types
	    List<CoreMap> sentences = document.get(SentencesAnnotation.class);

	    for(CoreMap sentence: sentences) {
	      // traversing the words in the current sentence
	      // a CoreLabel is a CoreMap with additional token-specific methods
	      for (CoreLabel token: sentence.get(TokensAnnotation.class)) {
	        token.get(TextAnnotation.class);
	        token.get(PartOfSpeechAnnotation.class);
	        token.get(NamedEntityTagAnnotation.class);

	      }

	      sentence.get(TreeAnnotation.class);

	      sentence.get(CollapsedCCProcessedDependenciesAnnotation.class);
	    }

//	    for (CoreLabel token : document.get(TokensAnnotation.class)) {
//	    	System.out.println(token.value() + "\t" + token.word() + "\t" + token.originalText() + "\t" + token.index() + "\t" + token.sentIndex());
//	    }

	    document.get(CorefChainAnnotation.class);

	    System.out.println("ok");

	}

}
