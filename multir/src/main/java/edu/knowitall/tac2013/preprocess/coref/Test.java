package edu.knowitall.tac2013.preprocess.coref;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import edu.stanford.nlp.dcoref.CorefChain;
import edu.stanford.nlp.dcoref.CorefCoreAnnotations;
import edu.stanford.nlp.ling.CoreAnnotation;
import edu.stanford.nlp.ling.CoreAnnotations;
import edu.stanford.nlp.ling.CoreAnnotations.CharacterOffsetBeginAnnotation;
import edu.stanford.nlp.ling.CoreAnnotations.CharacterOffsetEndAnnotation;
import edu.stanford.nlp.ling.CoreAnnotations.LemmaAnnotation;
import edu.stanford.nlp.ling.CoreAnnotations.NamedEntityTagAnnotation;
import edu.stanford.nlp.ling.CoreAnnotations.PartOfSpeechAnnotation;
import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.pipeline.Annotation;
import edu.stanford.nlp.pipeline.Annotator;
import edu.stanford.nlp.pipeline.StanfordCoreNLP;
import edu.stanford.nlp.trees.Tree;
import edu.stanford.nlp.trees.TreeCoreAnnotations.TreeAnnotation;
import edu.stanford.nlp.trees.Trees;
import edu.stanford.nlp.util.CoreMap;
import edu.stanford.nlp.util.ErasureUtils;

/*  java -cp ".:../lib/stanford-corenlp.jar:../lib/stanford-corenlp-models.jar:../lib/xom.jar:../lib/joda-time.jar" test.Test */


public class Test {

	static final String dir = "/projects/pardosa/data15/raphaelh/data";
	static final String in1 = dir + "/sentences.articleIDs";
	static final String in2 = dir + "/sentences.text";
	static final String in3 = dir + "/sentences.tokens";
	static final String in4 = dir + "/sentences.tokenSpans";
	static final String in5 = dir + "/sentences.cj";
	static final String in6 = dir + "/";

	static final String out1 = dir + "/sentences.stanfordposXX";
	static final String out2 = dir + "/sentences.stanfordlemmaXX";
	static final String out3 = dir + "/sentences.stanfordnerXX";
	static final String out4 = dir + "/sentences.stanfordcorefXX";

	//private static AnnotatorPool pool = null;

	public static void main(String[] args) throws Exception {
		Properties props = new Properties();
		//props.setProperty("annotators", "readrtext,pos,lemma,ner"); //,lemma,ner,dcoref"); //readrparse");

		boolean enforceRequirements = false;
		StanfordCoreNLP core = new StanfordCoreNLP(props, enforceRequirements);

		core.addAnnotator(new PseudoTextAnnotator(in2, in3, in4));
		core.addAnnotator(StanfordCoreNLP.getExistingAnnotator("pos"));
		core.addAnnotator(StanfordCoreNLP.getExistingAnnotator("lemma"));
		core.addAnnotator(StanfordCoreNLP.getExistingAnnotator("ner"));

		/*
		AnnotatorPool pool = getDefaultAnnotatorPool(props);

		pool.register("readrtext", new AnnotatorFactory(props) {
		      private static final long serialVersionUID = 1L;
		      @Override
		      public Annotator create() {
				return new PseudoTextAnnotator(in2, in3, in4);
		      }

		      @Override
		      public String signature() {
		    	  return null;
		      }
	    });
		pool.register("readrparse", new Factory<Annotator>() {
			private static final long serialVersionUID = 1L;
			public PseudoParseAnnotator create() {
				return new PseudoParseAnnotator(in5);
			}
		});
		*/

		BufferedWriter w1 = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(out1), "utf-8"));
		BufferedWriter w2 = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(out2), "utf-8"));
		BufferedWriter w3 = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(out3), "utf-8"));
		BufferedWriter w4 = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(out4), "utf-8"));

		AnnotationCreator c = new AnnotationCreator(in1);
		Annotation annotation = null;
		while ((annotation = c.next()) != null) {
			int docID = annotation.get(ReadrCoreAnnotations.DocIDAnnotation.class);
			if (docID % 1000 == 0) System.out.println("annotating doc " + docID);

			//if (docID < 132000) continue;
			if (docID < 132954) continue;

			System.out.println(docID);

			// TODO: error in doc 132954, sentence 2675998

			try {
				core.annotate(annotation);
			} catch (Exception e) {
				e.printStackTrace();
				// ignore error
			}


			// write to disk
			try {
				List<CoreMap> sentences = annotation.get(CoreAnnotations.SentencesAnnotation.class);
		        for (CoreMap sentence : sentences) {
					int sentenceID = sentence.get(ReadrCoreAnnotations.SentenceIDAnnotation.class);
		        	List<CoreLabel> tokens = sentence.get(CoreAnnotations.TokensAnnotation.class);

		        	// write pos
		        	w1.write(sentenceID + "");
		        	w1.write("\t");
		        	for (int i=0; i < tokens.size(); i++) {
		        		if (i > 0) w1.write(" ");
		        		w1.write(tokens.get(i).get(PartOfSpeechAnnotation.class));
		        	}
		        	w1.write("\n");

		        	// write lemma
		        	w2.write(sentenceID + "");
		        	w2.write("\t");
		        	for (int i=0; i < tokens.size(); i++) {
		        		if (i > 0) w2.write(" ");
		        		w2.write(tokens.get(i).get(LemmaAnnotation.class));
		        	}
		        	w2.write("\n");

		        	// write ner
		        	w3.write(sentenceID + "");
		        	w3.write("\t");
		        	for (int i=0; i < tokens.size(); i++) {
		        		if (i > 0) w3.write(" ");
		        		String ner = tokens.get(i).get(NamedEntityTagAnnotation.class);
		        		if (ner == null) break;
		        		w3.write(ner);
		        	}
		        	w3.write("\n");
		        }

		        Map<Integer, CorefChain> corefChains =
		            annotation.get(CorefCoreAnnotations.CorefChainAnnotation.class);
		        if (corefChains != null && sentences != null) {
		            /* -- still need to figure out how to serialize --
		        	List<List<CoreLabel>> sents = new ArrayList<List<CoreLabel>>();
		            for (CoreMap sentence : sentences) {
		              List<CoreLabel> tokens =
		                sentence.get(CoreAnnotations.TokensAnnotation.class);
		              sents.add(tokens);
		            }

		            for (CorefChain chain : corefChains.values()) {
		              CorefChain.CorefMention representative =
		                chain.getRepresentativeMention();
		              boolean outputHeading = false;
		              for (CorefChain.CorefMention mention : chain.getCorefMentions()) {
		                if (mention == representative)
		                  continue;
		                if (!outputHeading) {
		                  outputHeading = true;
		                  os.println("Coreference set:");
		                }
		                // all offsets start at 1!
		                os.println("\t(" + mention.sentNum + "," +
		                    mention.headIndex + ",[" +
		                    mention.startIndex + "," +
		                    mention.endIndex + ")) -> (" +
		                    representative.sentNum + "," +
		                    representative.headIndex + ",[" +
		                    representative.startIndex + "," +
		                    representative.endIndex + ")), that is: \"" +
		                    mention.mentionSpan + "\" -> \"" +
		                    representative.mentionSpan + "\"");
		              }
		            }
		          }

		          os.flush();
		          */
		        }

			} catch (Exception e) { throw new RuntimeException(e); } //throw new RuntimeException("unable to find words/tokens in: " + annotation); }
		}
		c.close();
		w1.close();
		w2.close();
		w3.close();
		w4.close();
	}


    // in wex, articleID is 3rd in sentences.meta
    // in nyt, articleID is 2nd in sentences.articleIDs
	static class AnnotationCreator {
		BufferedReader r;
		String nextLine = null;

		AnnotationCreator(String file) {
			try {
				r = new BufferedReader(new InputStreamReader(new FileInputStream(file), "utf-8"));
				nextLine = r.readLine();
			} catch (Exception e) { throw new RuntimeException(e); }
		}

		public Annotation next() {
			try {
				if (nextLine == null) return null;
				String[] c = nextLine.split("\t");
				int articleID = Integer.parseInt(c[1]);
				List<Integer> li = new ArrayList<Integer>();
				li.add(Integer.parseInt(c[0]));
				while ((nextLine = r.readLine()) != null &&
					(Integer.parseInt((c = nextLine.split("\t"))[1])) == articleID)
					li.add(Integer.parseInt(c[0]));
				Annotation annotation = new Annotation("");
				annotation.set(ReadrCoreAnnotations.DocIDAnnotation.class, articleID);
				annotation.set(ReadrCoreAnnotations.SentenceIDsAnnotation.class, li);
				return annotation;
			} catch (Exception e) { throw new RuntimeException(e); }
		}

		public void close() throws IOException {
			r.close();
		}
	}

    static class PseudoTextAnnotator implements Annotator {
    	private BufferedReader r1;
    	private BufferedReader r2;
    	private BufferedReader r3;

    	// text, tokens, tokenSpans
    	PseudoTextAnnotator(String file1, String file2, String file3) {
    		try {
	    		r1 = new BufferedReader(new InputStreamReader(new FileInputStream(file1), "utf-8"));
	    		r2 = new BufferedReader(new InputStreamReader(new FileInputStream(file2), "utf-8"));
	    		r3 = new BufferedReader(new InputStreamReader(new FileInputStream(file3), "utf-8"));
    		} catch (Exception e) { throw new RuntimeException(e); }
    	}

    	@Override
		public void annotate(Annotation annotation) {
    		try {
	    		List<Integer> li = annotation.get(ReadrCoreAnnotations.SentenceIDsAnnotation.class);
	    		StringBuilder sb = new StringBuilder();
	    		//int charOffset = 0;
	            int tokenOffset = 0;
	            List<CoreLabel> tokens = new ArrayList<CoreLabel>();
	            List<CoreMap> sentences = new ArrayList<CoreMap>();
        		//while ((sentenceID1 = Integer.parseInt(r1.readLine().split("\t")[0])) < li.get(0)


	    		for (int i=0; i < li.size(); i++) {
	    			int sentenceID = li.get(i);

	    			String l1, l2, l3;
	    			String[] c1, c2, c3;
	    			int sentenceID1, sentenceID2, sentenceID3;
	    			do {
		        		l1 = r1.readLine();
		        		c1 = l1.split("\t");
		        		sentenceID1 = Integer.parseInt(c1[0]);
	    			} while (sentenceID1 < sentenceID);
	        		if (sentenceID != sentenceID1)
	        			throw new RuntimeException("not aligned");

	        		do {
		        		l2 = r2.readLine();
		        		c2 = l2.split("\t");
		        		sentenceID2 = Integer.parseInt(c2[0]);
	        		} while (sentenceID2 < sentenceID);
	        		if (sentenceID != sentenceID2)
	        			throw new RuntimeException("not aligned");

	        		do {
		        		l3 = r3.readLine();
		        		c3 = l3.split("\t");
		        		sentenceID3 = Integer.parseInt(c3[0]);
	        		} while (sentenceID3 < sentenceID);
	        		if (sentenceID != sentenceID3)
	        			throw new RuntimeException("not aligned");

	        		// convert sentence tokens
	        		String[] t2 = c2[1].split(" ");
	        		String[] t3 = c3[1].split(" ");
	        		if (t2.length != t3.length) {
	        			for (int j=0; j < t2.length; j++)
	        				System.out.println(j + ": " + t2[j]);

	        			throw new RuntimeException("number of tokens mismatch for " + sentenceID1 + " " + t2.length + " != " + t3.length);
	        		}
	        		List<CoreLabel> sentenceTokens = new ArrayList<CoreLabel>(t2.length);
	        		int charOffset = sb.length();
	        		for (int j=0; j < t2.length; j++) {
		        		CoreLabel cl = new CoreLabel();
		        		String[] be = t3[j].split(":");
		        		int tb = Integer.parseInt(be[0]);
		        		int te = Integer.parseInt(be[1]);
		        		cl.setBeginPosition(charOffset + tb);
		        		cl.setEndPosition(charOffset + te);
		        		cl.setOriginalText(c1[1].substring(tb, te));
		        		cl.setSentIndex(-1);
		        		cl.setIndex(-1);
		        		cl.setValue(t2[j]);
		        		cl.setWord(t2[j]);
		        		sentenceTokens.add(cl);
	        		}
	        		tokens.addAll(sentenceTokens);

	                if (sentenceTokens.size() == 0) {
	                  throw new RuntimeException("unexpected empty sentence: " + sentenceTokens);
	                }

	                  // get the sentence text from the first and last character offsets
	                int begin = sentenceTokens.get(0).get(CharacterOffsetBeginAnnotation.class);
	                int last = sentenceTokens.size() - 1;
	                int end = sentenceTokens.get(last).get(CharacterOffsetEndAnnotation.class);

	                sb.append(c1[1]);
	                sb.append(" ");
	                String sentenceText = c1[1]; //text.substring(begin, end);

	                // create a sentence annotation with text and token offsets
	                Annotation sentence = new Annotation(sentenceText);
	                sentence.set(ReadrCoreAnnotations.SentenceIDAnnotation.class, sentenceID1);
	                sentence.set(CharacterOffsetBeginAnnotation.class, begin);
	                sentence.set(CharacterOffsetEndAnnotation.class, end);
	                sentence.set(CoreAnnotations.TokensAnnotation.class, sentenceTokens);
	                sentence.set(CoreAnnotations.TokenBeginAnnotation.class, tokenOffset);
	                tokenOffset += sentenceTokens.size();
	                sentence.set(CoreAnnotations.TokenEndAnnotation.class, tokenOffset);
	                // add the sentence to the list
	                sentences.add(sentence);
	            }

	    		annotation.set(CoreAnnotations.SentencesAnnotation.class, sentences);
	    		annotation.set(CoreAnnotations.TokensAnnotation.class, tokens);

	    		//printDocument(annotation);

    		} catch (Exception e) { throw new RuntimeException(e); }
    	}

    	public void close() throws IOException {
    		r1.close();
    		r2.close();
    		r3.close();
    	}
    }

    static class PseudoParseAnnotator implements Annotator {
    	private BufferedReader r1;

    	PseudoParseAnnotator(String file1) {
    		try {
    			r1 = new BufferedReader(new InputStreamReader(new FileInputStream(file1), "utf-8"));
    		} catch (Exception e) { throw new RuntimeException(e); }
    	}

    	@Override
		public void annotate(Annotation annotation) {
    		try {
	            for (CoreMap sentence: annotation.get(CoreAnnotations.SentencesAnnotation.class)) {
	            	int sentenceID = sentence.get(ReadrCoreAnnotations.SentenceIDAnnotation.class);

	    			String l1;
	    			String[] c1;
	    			int sentenceID1;
	    			do {
		        		l1 = r1.readLine();
		        		c1 = l1.split("\t");
		        		sentenceID1 = Integer.parseInt(c1[0]);
	    			} while (sentenceID1 < sentenceID);
	        		if (sentenceID != sentenceID1)
	        			throw new RuntimeException("not aligned");

	            	Tree tree = Trees.readTree(c1[1]);
	            	sentence.set(TreeAnnotation.class, tree);
	            }
    		} catch (Exception e) { throw new RuntimeException(e); }
    	}

    	public void close() throws IOException {
    		r1.close();
    	}
    }

    /*
    // copy/pasted from StanfordCoreNLP
    private static synchronized AnnotatorPool getDefaultAnnotatorPool(final Properties props) {
        // if the pool already exists reuse!
        if(pool != null) return pool;

        pool = new AnnotatorPool();

        //
        // POS tagger
        //
        pool.register(StanfordCoreNLP.STANFORD_POS, new Factory<Annotator>() {
          private static final long serialVersionUID = 1L;
          public Annotator create() {
            try {
              String maxLenStr = props.getProperty("pos.maxlen");
              int maxLen = Integer.MAX_VALUE;
              if(maxLenStr != null) maxLen = Integer.parseInt(maxLenStr);
              return new POSTaggerAnnotator(props.getProperty("pos.model", DefaultPaths.DEFAULT_POS_MODEL), true, maxLen);
            } catch (Exception e) {
              throw new RuntimeException(e);
            }
          }
        });

        //
        // Lemmatizer
        //
        pool.register(StanfordCoreNLP.STANFORD_LEMMA, new Factory<Annotator>() {
          private static final long serialVersionUID = 1L;
          public Annotator create() {
            return new MorphaAnnotator(false);
          }
        });

        //
        // NER
        //
        pool.register(StanfordCoreNLP.STANFORD_NER, new Factory<Annotator>() {
          private static final long serialVersionUID = 1L;
          public Annotator create() {
            List<String> models = new ArrayList<String>();
            List<Pair<String, String>> modelNames = new ArrayList<Pair<String,String>>();
            modelNames.add(new Pair<String, String>("ner.model", null));
            modelNames.add(new Pair<String, String>("ner.model.3class", DefaultPaths.DEFAULT_NER_THREECLASS_MODEL));
            modelNames.add(new Pair<String, String>("ner.model.7class", DefaultPaths.DEFAULT_NER_MUC_MODEL));
            modelNames.add(new Pair<String, String>("ner.model.MISCclass", DefaultPaths.DEFAULT_NER_CONLL_MODEL));

            for (Pair<String, String> name : modelNames) {
              String model = props.getProperty(name.first, name.second);
              if (model != null && model.length() > 0) {
                models.addAll(Arrays.asList(model.split(",")));
              }
            }
            if (models.isEmpty()) {
              throw new RuntimeException("no NER models specified");
            }
            NERClassifierCombiner nerCombiner;
            try {
              boolean applyNumericClassifiers =
                PropertiesUtils.getBool(props,
                    NERClassifierCombiner.APPLY_NUMERIC_CLASSIFIERS_PROPERTY,
                    NERClassifierCombiner.APPLY_NUMERIC_CLASSIFIERS_DEFAULT);
              boolean useSUTime =
                PropertiesUtils.getBool(props,
                    NumberSequenceClassifier.USE_SUTIME_PROPERTY,
                    NumberSequenceClassifier.USE_SUTIME_DEFAULT);
              nerCombiner = new NERClassifierCombiner(applyNumericClassifiers,
                    useSUTime, props,
                    models.toArray(new String[models.size()]));
            } catch (FileNotFoundException e) {
              throw new RuntimeException(e);
            }
            // ms 2009, no longer needed: the functionality of all these annotators is now included in NERClassifierCombiner
//            AnnotationPipeline pipeline = new AnnotationPipeline();
//            pipeline.addAnnotator(new NERCombinerAnnotator(nerCombiner, false));
//            pipeline.addAnnotator(new NumberAnnotator(false));
//            pipeline.addAnnotator(new TimeWordAnnotator(false));
//            pipeline.addAnnotator(new QuantifiableEntityNormalizingAnnotator(false, false));
//            return pipeline;
            return new NERCombinerAnnotator(nerCombiner, false);
          }
        });

        //
        // Regex NER
        //
        pool.register(StanfordCoreNLP.STANFORD_REGEXNER, new Factory<Annotator>() {
          private static final long serialVersionUID = 1L;
          public Annotator create() {
            String mapping = props.getProperty("regexner.mapping", DefaultPaths.DEFAULT_REGEXNER_RULES);
            String ignoreCase = props.getProperty("regexner.ignorecase", "false");
            String validPosPattern = props.getProperty("regexner.validpospattern", RegexNERSequenceClassifier.DEFAULT_VALID_POS);
            return new RegexNERAnnotator(mapping, Boolean.valueOf(ignoreCase), validPosPattern);
          }
        });

        //
        // Gender Annotator
        //
        pool.register(StanfordCoreNLP.STANFORD_GENDER, new Factory<Annotator>() {
          private static final long serialVersionUID = 1L;
          public Annotator create() {
            return new GenderAnnotator(false, props.getProperty("gender.firstnames", DefaultPaths.DEFAULT_GENDER_FIRST_NAMES));
          }
        });


        //
        // True caser
        //
        pool.register(StanfordCoreNLP.STANFORD_TRUECASE, new Factory<Annotator>() {
          private static final long serialVersionUID = 1L;
          public Annotator create() {
            String model = props.getProperty("truecase.model", DefaultPaths.DEFAULT_TRUECASE_MODEL);
            String bias = props.getProperty("truecase.bias", TrueCaseAnnotator.DEFAULT_MODEL_BIAS);
            String mixed = props.getProperty("truecase.mixedcasefile", DefaultPaths.DEFAULT_TRUECASE_DISAMBIGUATION_LIST);
            return new TrueCaseAnnotator(model, bias, mixed, false);
          }
        });

        //
        // Post-processing tokenization rules for the NFL domain
        //
//        pool.register(StanfordCoreNLP.STANFORD_NFL_TOKENIZE, new Factory<Annotator>() {
//          private static final long serialVersionUID = 1L;
//          public Annotator create() {
//            final String className =
//              "edu.stanford.nlp.pipeline.NFLTokenizerAnnotator";
//            return ReflectionLoading.loadByReflection(className);
//          }
//        });

        //
        // Entity and relation extraction for the NFL domain
        //
        pool.register(StanfordCoreNLP.STANFORD_NFL, new Factory<Annotator>() {
          private static final long serialVersionUID = 1L;
          public Annotator create() {
            // these paths now extracted inside c'tor
            // String gazetteer = props.getProperty("nfl.gazetteer", DefaultPaths.DEFAULT_NFL_GAZETTEER);
            // String entityModel = props.getProperty("nfl.entity.model", DefaultPaths.DEFAULT_NFL_ENTITY_MODEL);
            // String relationModel = props.getProperty("nfl.relation.model", DefaultPaths.DEFAULT_NFL_RELATION_MODEL);
            final String className = "edu.stanford.nlp.pipeline.NFLAnnotator";
            return ReflectionLoading.loadByReflection(className, props);
          }
        });

        //
        // Parser
        //
        pool.register(StanfordCoreNLP.STANFORD_PARSE, new Factory<Annotator>() {
          private static final long serialVersionUID = 1L;
          public Annotator create() {
            String parserType = props.getProperty("parser.type", "stanford");
            String maxLenStr = props.getProperty("parser.maxlen");

            if (parserType.equalsIgnoreCase("stanford")) {
              int maxLen = -1;
              if (maxLenStr != null) {
                maxLen = Integer.parseInt(maxLenStr);
              }
              String parserPath = props.getProperty("parser.model",
                                        DefaultPaths.DEFAULT_PARSER_MODEL);
              boolean parserDebug =
                PropertiesUtils.hasProperty(props, "parser.debug");
              String parserFlags = props.getProperty("parser.flags");
              String[] parserFlagList =
                ParserAnnotator.convertFlagsToArray(parserFlags);
              ParserAnnotator anno = new ParserAnnotator(parserPath, parserDebug,
                                                         maxLen, parserFlagList);
              return anno;
            } else if (parserType.equalsIgnoreCase("charniak")) {
              String model = props.getProperty("parser.model");
              String parserExecutable = props.getProperty("parser.executable");
              if (model == null || parserExecutable == null) {
                throw new RuntimeException("Both parser.model and parser.executable properties must be specified if parser.type=charniak");
              }
              int maxLen = 399;
              if (maxLenStr != null) {
                maxLen = Integer.parseInt(maxLenStr);
              }

              CharniakParserAnnotator anno = new CharniakParserAnnotator(model, parserExecutable, false, maxLen);

              return anno;
            } else {
              throw new RuntimeException("Unknown parser type: " + parserType + " (currently supported: stanford and charniak)");
            }
          }
        });

        //
        // Coreference resolution
        //
        pool.register(StanfordCoreNLP.STANFORD_DETERMINISTIC_COREF, new Factory<Annotator>() {
          private static final long serialVersionUID = 1L;
          public Annotator create() {
            return new DeterministicCorefAnnotator(props);
          }
        });

        return pool;
    }
    */

    static class ReadrCoreAnnotations {
    	public static class DocIDAnnotation implements CoreAnnotation<Integer> {
    		@Override
			public Class<Integer> getType() {
    			return Integer.class;
    		}
    	}
    	public static class SentenceIDsAnnotation implements CoreAnnotation<List<Integer>> {
    		@Override
			public Class<List<Integer>> getType() {
    			return ErasureUtils.<Class<List<Integer>>> uncheckedCast(List.class);
    		}
    	}
    	public static class SentenceIDAnnotation implements CoreAnnotation<Integer> {
    		@Override
			public Class<Integer> getType() {
    			return Integer.class;
    		}
    	}
    }
}
