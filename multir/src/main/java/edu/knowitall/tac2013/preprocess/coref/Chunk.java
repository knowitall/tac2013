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

// mvn install:install-file -DlocalRepository=repo -DcreateChecksum=true -Dpackaging=jar -Dfile=lib/stanford-corenlp.jar -DgroupId=edu.stanford.nlp -DartifactId=stanford-corenlp -Dversion=1.3.4
// mvn install:install-file -DlocalRepository=repo -DcreateChecksum=true -Dpackaging=jar -Dfile=lib/stanford-corenlp-models.jar -DgroupId=edu.stanford.nlp -DartifactId=stanford-corenlp-models -Dversion=1.3.4

/*  java -cp ".:../lib/stanford-corenlp.jar:../lib/stanford-corenlp-models.jar:../lib/xom.jar:../lib/joda-time.jar" test.Test */
//mvn exec:java -Dexec.mainClass="data.preprocess.coref.Chunk" -Dexec.args=""

public class Chunk {

	// static final String dir = "/projects/pardosa/data15/raphaelh/data";
	// static final String dir = "/projects/pardosa/data15/raphaelh/data/tmp";
	// static final String dir =
	// "/projects/pardosa/data15/raphaelh/readr2exp/ace05";
	// static final String dir = "/projects/pardosa/data15/raphaelh/roth-data";
	// static final String dir = "/projects/pardosa/data15/raphaelh/biology";
	static final String inDir = "/projects/pardosa/s5/raphaelh/tac/data/09nw";
	static String in1 = inDir + "/sentences.articleIDs";
	static String in2 = inDir + "/sentences.text";
	static String in3 = inDir + "/sentences.tokens";
	static String in4 = inDir + "/sentences.tokenSpans";
	static String in5 = inDir + "/sentences.cj";
	static String in6 = inDir + "/";

	// separate input and output directories
	static final String outDir = "/projects/pardosa/s5/TACKBP/data/09nw";
	static String out1 = outDir + "/sentences.stanfordpos";
	static String out2 = outDir + "/sentences.stanfordlemma";
	static String out3 = outDir + "/sentences.stanfordner";
	static String out4 = outDir + "/sentences.stanfordcoref";
	static String out5 = outDir + "/sentences.errors";

	public static void main(String[] args) throws Exception {
		// String sfx = args[0];
		String sfx = "";
		in1 += sfx;
		in2 += sfx;
		in3 += sfx;
		in4 += sfx;
		in5 += sfx;
		out1 += sfx;
		out2 += sfx;
		out3 += sfx;
		out4 += sfx;
		out5 += sfx;

		Properties props = new Properties();
		props.setProperty("annotators", "readrtext,pos,lemma,ner"); // readrtext,pos,lemma,ner,lemma,ner,dcoref"); //readrparse");
		// adds a custom Annotator class.
		// data.preprocess.coref.PseudoTextAnnotator now is a separate class.
		props.setProperty("customAnnotatorClass.readrtext",
				"data.preprocess.coref.PseudoTextAnnotator");
		props.setProperty("readrtext.in1", in2);
		props.setProperty("readrtext.in2", in3);
		props.setProperty("readrtext.in3", in4);
		boolean enforceRequirements = false;
		StanfordCoreNLP core = new StanfordCoreNLP(props, enforceRequirements);

		// core.addAnnotator(new PseudoTextAnnotator(in2, in3, in4));
		// core.addAnnotator(StanfordCoreNLP.getExistingAnnotator("pos"));
		// core.addAnnotator(StanfordCoreNLP.getExistingAnnotator("lemma"));
		// core.addAnnotator(StanfordCoreNLP.getExistingAnnotator("ner"));

		BufferedWriter w1 = new BufferedWriter(new OutputStreamWriter(
				new FileOutputStream(out1), "utf-8"));
		BufferedWriter w2 = new BufferedWriter(new OutputStreamWriter(
				new FileOutputStream(out2), "utf-8"));
		BufferedWriter w3 = new BufferedWriter(new OutputStreamWriter(
				new FileOutputStream(out3), "utf-8"));
		BufferedWriter w4 = new BufferedWriter(new OutputStreamWriter(
				new FileOutputStream(out4), "utf-8"));
		BufferedWriter w5 = new BufferedWriter(new OutputStreamWriter(
				new FileOutputStream(out5), "utf-8"));

		AnnotationCreator c = new AnnotationCreator(in1);
		Annotation annotation = null;
		while ((annotation = c.next()) != null) {
			int docID = annotation
					.get(ReadrCoreAnnotations.DocIDAnnotation.class);
			if (docID % 1000 == 0)
				System.out.println("annotating doc " + docID);

			// checkpoints
			// if (docID < 1122930) continue;
			// if (docID < 132954) continue;

			// System.out.println(docID);

			// TODO: error in doc 132954, sentence 2675998

			try {
				core.annotate(annotation);
			} catch (Exception e) {
				e.printStackTrace();
				// ignore error
			}

			// write to disk
			try {
				List<CoreMap> sentences = annotation
						.get(CoreAnnotations.SentencesAnnotation.class);
				for (CoreMap sentence : sentences) {
					int sentenceID = sentence
							.get(ReadrCoreAnnotations.SentenceIDAnnotation.class);
					List<CoreLabel> tokens = sentence
							.get(CoreAnnotations.TokensAnnotation.class);

					// write pos
					w1.write(sentenceID + "");
					w1.write("\t");
					for (int i = 0; i < tokens.size(); i++) {
						if (i > 0)
							w1.write(" ");
						w1.write(tokens.get(i)
								.get(PartOfSpeechAnnotation.class));
					}
					w1.write("\n");

					// write lemma
					w2.write(sentenceID + "");
					w2.write("\t");
					for (int i = 0; i < tokens.size(); i++) {
						if (i > 0)
							w2.write(" ");
						w2.write(tokens.get(i).get(LemmaAnnotation.class));
					}
					w2.write("\n");

					// write ner
					w3.write(sentenceID + "");
					w3.write("\t");
					for (int i = 0; i < tokens.size(); i++) {
						if (i > 0)
							w3.write(" ");
						String ner = tokens.get(i).get(
								NamedEntityTagAnnotation.class);
						if (ner == null) {
							w5.write(sentenceID + "\n");
							break;
						}
						w3.write(ner);
					}
					w3.write("\n");
				}

				Map<Integer, CorefChain> corefChains = annotation
						.get(CorefCoreAnnotations.CorefChainAnnotation.class);
				if (corefChains != null && sentences != null) {
					/*
					 * -- still need to figure out how to serialize --
					 * List<List<CoreLabel>> sents = new
					 * ArrayList<List<CoreLabel>>();
					 * for (CoreMap sentence : sentences) {
					 * List<CoreLabel> tokens =
					 * sentence.get(CoreAnnotations.TokensAnnotation.class);
					 * sents.add(tokens);
					 * }
					 *
					 * for (CorefChain chain : corefChains.values()) {
					 * CorefChain.CorefMention representative =
					 * chain.getRepresentativeMention();
					 * boolean outputHeading = false;
					 * for (CorefChain.CorefMention mention :
					 * chain.getCorefMentions()) {
					 * if (mention == representative)
					 * continue;
					 * if (!outputHeading) {
					 * outputHeading = true;
					 * os.println("Coreference set:");
					 * }
					 * // all offsets start at 1!
					 * os.println("\t(" + mention.sentNum + "," +
					 * mention.headIndex + ",[" +
					 * mention.startIndex + "," +
					 * mention.endIndex + ")) -> (" +
					 * representative.sentNum + "," +
					 * representative.headIndex + ",[" +
					 * representative.startIndex + "," +
					 * representative.endIndex + ")), that is: \"" +
					 * mention.mentionSpan + "\" -> \"" +
					 * representative.mentionSpan + "\"");
					 * }
					 * }
					 * }
					 *
					 * os.flush();
					 */
				}

			} catch (Exception e) {
				throw new RuntimeException(e);
			} // throw new RuntimeException("unable to find words/tokens in: " +
				// annotation); }
		}
		c.close();
		w1.close();
		w2.close();
		w3.close();
		w4.close();
		w5.close();
	}

	// in wex, articleID is 3rd in sentences.meta
	// in nyt, articleID is 2nd in sentences.articleIDs
	static class AnnotationCreator {
		BufferedReader r;
		String nextLine = null;

		AnnotationCreator(String file) {
			try {
				r = new BufferedReader(new InputStreamReader(
						new FileInputStream(file), "utf-8"));
				nextLine = r.readLine();
			} catch (Exception e) {
				throw new RuntimeException(e);
			}
		}

		public Annotation next() {
			try {
				if (nextLine == null)
					return null;
				String[] c = nextLine.split("\t");
				int articleID = Integer.parseInt(c[1]);
				List<Integer> li = new ArrayList<Integer>();
				li.add(Integer.parseInt(c[0]));
				while ((nextLine = r.readLine()) != null
						&& (Integer
								.parseInt((c = nextLine.split("\t"))[1])) == articleID)
					li.add(Integer.parseInt(c[0]));
				Annotation annotation = new Annotation("");
				annotation.set(ReadrCoreAnnotations.DocIDAnnotation.class,
						articleID);
				annotation.set(
						ReadrCoreAnnotations.SentenceIDsAnnotation.class, li);
				return annotation;
			} catch (Exception e) {
				throw new RuntimeException(e);
			}
		}

		public void close() throws IOException {
			r.close();
		}
	}

	static class PseudoParseAnnotator implements Annotator {
		private BufferedReader r1;

		PseudoParseAnnotator(String file1) {
			try {
				r1 = new BufferedReader(new InputStreamReader(
						new FileInputStream(file1), "utf-8"));
			} catch (Exception e) {
				throw new RuntimeException(e);
			}
		}

		@Override
		public void annotate(Annotation annotation) {
			try {
				for (CoreMap sentence : annotation
						.get(CoreAnnotations.SentencesAnnotation.class)) {
					int sentenceID = sentence
							.get(ReadrCoreAnnotations.SentenceIDAnnotation.class);

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
			} catch (Exception e) {
				throw new RuntimeException(e);
			}
		}

		public void close() throws IOException {
			r1.close();
		}
	}

	static class ReadrCoreAnnotations {
		public static class DocIDAnnotation implements CoreAnnotation<Integer> {
			@Override
			public Class<Integer> getType() {
				return Integer.class;
			}
		}

		public static class SentenceIDsAnnotation implements
				CoreAnnotation<List<Integer>> {
			@Override
			public Class<List<Integer>> getType() {
				return ErasureUtils
						.<Class<List<Integer>>> uncheckedCast(List.class);
			}
		}

		public static class SentenceIDAnnotation implements
				CoreAnnotation<Integer> {
			@Override
			public Class<Integer> getType() {
				return Integer.class;
			}
		}
	}
}
