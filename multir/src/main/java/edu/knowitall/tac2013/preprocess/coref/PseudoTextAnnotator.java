package edu.knowitall.tac2013.preprocess.coref;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import edu.knowitall.tac2013.preprocess.coref.Chunk.ReadrCoreAnnotations;
import edu.stanford.nlp.ling.CoreAnnotations;
import edu.stanford.nlp.ling.CoreAnnotations.CharacterOffsetBeginAnnotation;
import edu.stanford.nlp.ling.CoreAnnotations.CharacterOffsetEndAnnotation;
import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.pipeline.Annotation;
import edu.stanford.nlp.pipeline.Annotator;
import edu.stanford.nlp.util.CoreMap;

/**
 * It used to be a static class in Chunk.
 * No significant changes except an additional constructor.
 * @author xiaoling
 *
 */
public class PseudoTextAnnotator implements Annotator {
	private BufferedReader r1;
	private BufferedReader r2;
	private BufferedReader r3;

	public PseudoTextAnnotator(String annotatorName, Properties props) {
		try {
			r1 = new BufferedReader(new InputStreamReader(
					new FileInputStream(props.getProperty(annotatorName+".in1")), "utf-8"));
			r2 = new BufferedReader(new InputStreamReader(
					new FileInputStream(props.getProperty(annotatorName+".in2")), "utf-8"));
			r3 = new BufferedReader(new InputStreamReader(
					new FileInputStream(props.getProperty(annotatorName+".in3")), "utf-8"));
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}
	// text, tokens, tokenSpans
	PseudoTextAnnotator(String file1, String file2, String file3) {
		try {
			r1 = new BufferedReader(new InputStreamReader(
					new FileInputStream(file1), "utf-8"));
			r2 = new BufferedReader(new InputStreamReader(
					new FileInputStream(file2), "utf-8"));
			r3 = new BufferedReader(new InputStreamReader(
					new FileInputStream(file3), "utf-8"));
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	@Override
	public void annotate(Annotation annotation) {
		try {
//			System.out.println("===========annotating..=======");
			List<Integer> li = annotation
					.get(ReadrCoreAnnotations.SentenceIDsAnnotation.class);
			StringBuilder sb = new StringBuilder();
			// int charOffset = 0;
			int tokenOffset = 0;
			List<CoreLabel> tokens = new ArrayList<CoreLabel>();
			List<CoreMap> sentences = new ArrayList<CoreMap>();
			// while ((sentenceID1 =
			// Integer.parseInt(r1.readLine().split("\t")[0])) < li.get(0)

			for (int i = 0; i < li.size(); i++) {
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
				    // check if <...> is in the sentence
				    List<String> largeTokens = new ArrayList<String>();
				    for (int j=0; j < t2.length; j++){
					if (t2[j].startsWith("<")) {
					    StringBuilder largeToken = new StringBuilder();
					    for ( ; j < t2.length; j++){
						largeToken.append(t2[j]+(char)0x00A0);
						if (t2[j].endsWith(">")){
						    break;
						}
					    }
					    largeToken.deleteCharAt(largeToken.length()-1);
					    largeTokens.add(largeToken.toString());
					} else {
					    largeTokens.add(t2[j]);
					}
				    }
				    if (largeTokens.size() == t3.length) {
					t2 = largeTokens.toArray(new String[0]);
					System.out.println("[ERROR] <> tokens at "+sentenceID1);
				    } else {
					for (int j=0; j < t2.length; j++)
					    System.out.println(j + ": " + t2[j]);

					throw new RuntimeException("number of tokens mismatch for " + sentenceID1 + " " + t2.length + " != " + t3.length);
				    }
				}
				List<CoreLabel> sentenceTokens = new ArrayList<CoreLabel>(
						t2.length);
				int charOffset = sb.length();
				for (int j = 0; j < t2.length; j++) {
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
					throw new RuntimeException(
							"unexpected empty sentence: " + sentenceTokens);
				}

				// get the sentence text from the first and last character
				// offsets
				int begin = sentenceTokens.get(0).get(
						CharacterOffsetBeginAnnotation.class);
				int last = sentenceTokens.size() - 1;
				int end = sentenceTokens.get(last).get(
						CharacterOffsetEndAnnotation.class);

				sb.append(c1[1]);
				sb.append(" ");
				String sentenceText = c1[1]; // text.substring(begin, end);

				// create a sentence annotation with text and token offsets
				Annotation sentence = new Annotation(sentenceText);
				sentence.set(
						ReadrCoreAnnotations.SentenceIDAnnotation.class,
						sentenceID1);
				sentence.set(CharacterOffsetBeginAnnotation.class, begin);
				sentence.set(CharacterOffsetEndAnnotation.class, end);
				sentence.set(CoreAnnotations.TokensAnnotation.class,
						sentenceTokens);
				sentence.set(CoreAnnotations.TokenBeginAnnotation.class,
						tokenOffset);
				tokenOffset += sentenceTokens.size();
				sentence.set(CoreAnnotations.TokenEndAnnotation.class,
						tokenOffset);
				// add the sentence to the list
				sentences.add(sentence);
			}

			annotation.set(CoreAnnotations.SentencesAnnotation.class,
					sentences);
			annotation.set(CoreAnnotations.TokensAnnotation.class, tokens);

			// printDocument(annotation);
//			System.out.println("finished..");
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	public void close() throws IOException {
		r1.close();
		r2.close();
		r3.close();
	}
}
