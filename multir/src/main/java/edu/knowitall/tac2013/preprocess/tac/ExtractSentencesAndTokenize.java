package edu.knowitall.tac2013.preprocess.tac;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.StringReader;
import java.util.List;

import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.process.CoreLabelTokenFactory;
import edu.stanford.nlp.process.LexedTokenFactory;
import edu.stanford.nlp.process.PTBTokenizer;
import edu.stanford.nlp.process.WordToSentenceProcessor;

public class ExtractSentencesAndTokenize {

	static String inDir = "/projects/pardosa/s5/raphaelh/tac";

	static String in0 = inDir + "/data/10wb/paragraphs.meta";
	static String in1 = inDir + "/data/10wb/paragraphs.text";

	static String outDir = "/projects/pardosa/s5/TACKBP";
	static String out0 = outDir + "/data/10wb/sentences.meta";
	static String out1 = outDir + "/sdata/10wb/sentences.text";
	static String out2 = outDir + "/data/10wb/sentences.tokens";
	static String out3 = outDir + "/data/10wb/sentences.tokenSpans";

	public static void main(String[] args) throws IOException {
		String options = "invertible=true,ptb3Escaping=true";

		LexedTokenFactory<CoreLabel> ltf = new CoreLabelTokenFactory(true);

		WordToSentenceProcessor<CoreLabel> sen = new WordToSentenceProcessor<CoreLabel>();

		BufferedWriter w0 = new BufferedWriter(new OutputStreamWriter(
				new FileOutputStream(out0), "utf-8"));
		BufferedWriter w1 = new BufferedWriter(new OutputStreamWriter(
				new FileOutputStream(out1), "utf-8"));
		BufferedWriter w2 = new BufferedWriter(new OutputStreamWriter(
				new FileOutputStream(out2), "utf-8"));
		BufferedWriter w3 = new BufferedWriter(new OutputStreamWriter(
				new FileOutputStream(out3), "utf-8"));

		BufferedReader r0 = new BufferedReader(new InputStreamReader(
				new FileInputStream(in0), "utf-8"));
		BufferedReader r1 = new BufferedReader(new InputStreamReader(
				new FileInputStream(in1), "utf-8"));
		String line0 = null, line1 = null;
		int sentenceID = 0;
		while ((line0 = r0.readLine()) != null
				&& (line1 = r1.readLine()) != null) {
			String[] t0 = line0.split("\t");
			String articleID = t0[1];
			int parBeginOffset = Integer.parseInt(t0[2]);
			// int parEndOffset = Integer.parseInt(t0[3]);
			String[] t1 = line1.split("\t");
			String paragraphID = t1[0];
			String par = t1[1];

			// Clean up for htmls, urls, emails, etc (Xiao)
			// NOTE: the offsets are no longer w.r.t the original text
			// TODO at some point, I need to collect stats about how many of
			// the replacements took place, sample some and verify the
			// correctness.
			par = par
			// replace urls
					.replaceAll("https?://\\S+?(\\s|$)", "U_R_L$1")
					// replace emails
					.replaceAll(
							"[A-Za-z0-9\\.\\-]+?@([A-Za-z0-9\\-]+?\\.){1,}+(com|net)",
							"E_M_A_I_L")
					// replace "<a ... @xxx.yyy>" emails
					.replaceAll(
							"<[A-Za-z0-9\\.\\-]+? [\\.]{3} @([A-Za-z0-9\\-]+?\\.){1,}+(com|net)>",
							"E_M_A_I_L")
					// replace long dashes
					.replaceAll("[\\-_=]{3,}+", "---")
					// replace all utf8 spaces to the simplest space
					// Note (Rob Bart 6/12/13): The original code used
					// StringUtil.whitespace_charclass
					// But there was no hint as to where 'StringUtil' was supposed to
					// have come from. I googled 'StringUtil.whitespace_charclass'
					// and came up with something reasonable - see the comments around
					// this.whitespace_charclass.
					.replaceAll(whitespace_charclass, " ")
					// e.g. "</a" is left as a token due to bad html writing
					.replaceAll("</\\p{Alnum}", "");
			par = HtmlUtils.removeHtml(par).replace("[ \t\\u000B\f\r]+", " ");
			// END clean up

			PTBTokenizer<CoreLabel> tok = new PTBTokenizer<CoreLabel>(
					new StringReader(par), ltf, options);
			List<CoreLabel> l = tok.tokenize();

			List<List<CoreLabel>> snts = sen.process(l);

			for (int s = 0; s < snts.size(); s++) {
				List<CoreLabel> snt = snts.get(s);

				StringBuilder sb2 = new StringBuilder();
				StringBuilder sb3 = new StringBuilder();

				int sntBegin = snt.get(0).beginPosition();
				for (CoreLabel cl : snt) {
					if (sb2.length() > 0)
						sb2.append(" ");
					// To avoid stanford corenlp group words
					// into a huge word between angle brackets,
					// which will confuse the cj parser (Xiao)
					if (cl.word().startsWith("<") && cl.word().endsWith(">")) {
						sb2.append("LONG_WORD");
					} else {
						sb2.append(cl.word());
					}

					if (sb3.length() > 0)
						sb3.append(" ");
					int from = cl.beginPosition() - sntBegin;
					int to = cl.endPosition() - sntBegin;
					sb3.append(from + ":" + to);
				}

				String rawSnt = par.substring(sntBegin, snt.get(snt.size() - 1)
						.endPosition());

				w0.write(sentenceID
						+ "\t"
						+ paragraphID
						+ "\t"
						+ articleID
						+ "\t"
						+ (parBeginOffset + sntBegin)
						+ "\t"
						+ (parBeginOffset + snt.get(snt.size() - 1)
								.endPosition()) + "\n");

				w1.write(sentenceID + "\t" + rawSnt + "\n");

				// w0.write(sentenceID + "\t" + articleID + "\n");
				// w1.write(sentenceID + "\t" + PTBTokenizer.labelList2Text(snt)
				// + "\n");

				w2.write(sentenceID + "\t" + sb2.toString() + "\n");
				w3.write(sentenceID + "\t" + sb3.toString() + "\n");
				sentenceID++;
				if (sentenceID % 10000 == 0) {
					System.out.println("# sentence processed = " + sentenceID);
				}
			}

			// if (sentenceID > 10000)
			// break;
		}
		r0.close();
		r1.close();
		w0.close();
		w1.close();
		w2.close();
		w3.close();
	}




	/*
	 * Note:
	 * The following fields whitespace_chars and whitespace_charclass
	 * are taken from
	 * http://bionlp.sourceforge.net/common/apidocs/v1.2/index.html?edu/ucdenver/ccp/common/string/RegExUnicodeUtils.html
	 * Where whitespace_charclass is now used, the original code called StringUtil.whitespace_charclass.
	 * The link above seems to be where this probably came from, and lets us avoid depending
	 * on another big project.
	 */

	/*
	 * Because Java's \s and \S and \p{Space} are all unusable.
	 */
	private final static String whitespace_chars = "" /*
													 * dummy empty string for
													 * homogeneity
													 */
			+ "\\u000A" // LINE FEED (LF)
			+ "\\u000B" // LINE TABULATION
			+ "\\u000C" // FORM FEED (FF)
			+ "\\u000D" // CARRIAGE RETURN (CR)
			+ "\\u0020" // SPACE
			+ "\\u0085" // NEXT LINE (NEL)
			+ "\\u00A0" // NO-BREAK SPACE
			+ "\\u1680" // OGHAM SPACE MARK
			+ "\\u180E" // MONGOLIAN VOWEL SEPARATOR
			+ "\\u2000" // EN QUAD
			+ "\\u2001" // EM QUAD
			+ "\\u2002" // EN SPACE
			+ "\\u2003" // EM SPACE
			+ "\\u2004" // THREE-PER-EM SPACE
			+ "\\u2005" // FOUR-PER-EM SPACE
			+ "\\u2006" // SIX-PER-EM SPACE
			+ "\\u2007" // FIGURE SPACE
			+ "\\u2008" // PUNCTUATION SPACE
			+ "\\u2009" // THIN SPACE
			+ "\\u200A" // HAIR SPACE
			+ "\\u2028" // LINE SEPARATOR
			+ "\\u2029" // PARAGRAPH SEPARATOR
			+ "\\u202F" // NARROW NO-BREAK SPACE
			+ "\\u205F" // MEDIUM MATHEMATICAL SPACE
			+ "\\u3000" // IDEOGRAPHIC SPACE
	;

	public final static String whitespace_charclass = "[" + whitespace_chars + "]";

}
