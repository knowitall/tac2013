package edu.knowitall.tac2013.preprocess.coref;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

public class RewriteCoref7 {

	//static String dir = "/scratch3/raphaelh/scratch2/nythalfd_dev";
	//static String dir = "/projects/pardosa/data15/raphaelh/readr2exp/nythalfd_dev";
	//static String dir = "/projects/pardosa/data15/raphaelh/readr2exp/nythalfd_test";
	//static final String dir = "/projects/pardosa/data15/raphaelh/readr2exp/ace05";
	//static final String dir = "/projects/pardosa/data15/raphaelh/roth-data";
	static final String dir = "/projects/pardosa/data15/raphaelh/biology";


	static String in = dir + "/sentences.stanfordcoref";
	static String out1 = dir + "/sentences.stanfordcoref.X.all";
	static String out2 = dir + "/sentences.stanfordcoref.X.proper";
	static String out3 = dir + "/sentences.stanfordcoref.X.proper.rep";

//	static String in = "/projects/pardosa/data15/raphaelh/readr2exp/nythalfd_test/sentences.stanfordcoref";
//	static String out1 = "/projects/pardosa/data15/raphaelh/readr2exp/nythalfd_test/sentences.stanfordcoref.all";
//	static String out2 = "/projects/pardosa/data15/raphaelh/readr2exp/nythalfd_test/sentences.stanfordcoref.proper";

	// COLUMNS: mentionID, sentenceID, headPos, repKey

	public static void main(String[] args) throws IOException {
		plan1();

	}
	/*
	public static void old() throws IOException {
		// read blocks of coreferences (chains) and determine the most representative of each

//		BufferedWriter w1 = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(out1)));
//		BufferedWriter w2 = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(out2)));
		BufferedReader r = new BufferedReader(new InputStreamReader(new FileInputStream(in)));
		String l = null;
		List<String[]> block = new ArrayList<String[]>();
		while ((l = r.readLine())!= null) {
			String[] c = l.split("\t");
			// same document and same chain
			if (block.isEmpty() ||
					(block.get(0)[0].equals(c[0]) && block.get(0)[1].equals(c[1]))) {
				block.add(c);
			} else {
				// process block
				process(block); //, w1, w2);
				block.clear();
				block.add(c);
			}
		}
		if (!block.isEmpty())
			process(block); //, w1, w2);
		r.close();
//		w1.close();
//		w2.close();
	}

	static int mentionID = 0;
	//static int chain = 0;

	// docID corefClusterID mentionID sentenceID sentNum start end headIndex position mentionSpan mentionType number gender animacy isRepresentative

//	in[8].setInt(0, ); //mentionID
//	in[8].setInt(1, sentenceID); //sentenceID
//	in[8].setByte(2, Byte.parseByte(c[5])); //start
//	in[8].setByte(3, Byte.parseByte(c[6])); //end
//	in[8].setByte(4, Byte.parseByte(c[7])); //head
//	in[8].setInt(5, ); //representativeMentionID
//	in[8].addBatch();

	/*
	private static void process(List<String[]> block) throws IOException {
		String[] rep = null;
		//int repStartPos=-1, repEndPos=-1, repHeadPos=-1;

		for (int j=0; j < block.size(); j++)
			if (block.get(j)[14].equals("true")) {
				//repID = mentionID + j;
				rep = block.get(j);
				//repStartPos = Integer.parseInt(rep[5]) - 1;
				//repEndPos = Integer.parseInt(rep[6]) - 1;
				//repHeadPos = Integer.parseInt(rep[7]) - 1;
			}

		System.out.println("-----------");
		for (int j=0; j < block.size(); j++) {
			if (block.get(j)[14].equals("true"))
				System.out.print("*** ");
			if (block.get(j)[10].equals("PROPER"))
				System.out.print("PROPER ");
			System.out.println(block.get(j)[9]);
		}
	}

	/*
	private static void process(List<String[]> block, BufferedWriter w1,
			BufferedWriter w2) throws IOException {
		int repID = -1;
		//int representativeSentenceID = -1;
		//int
		String[] rep = null;
		int repStartPos=-1, repEndPos=-1, repHeadPos=-1;

		for (int j=0; j < block.size(); j++)
			if (block.get(j)[14].equals("true")) {
				repID = mentionID + j;
				rep = block.get(j);
				repStartPos = Integer.parseInt(rep[5]) - 1;
				repEndPos = Integer.parseInt(rep[6]) - 1;
				repHeadPos = Integer.parseInt(rep[7]) - 1;
			}

		// if it is not proper, ignore
		//if (!representativeMention[10].equals("PROPER")) return;

		// find shortest mention with same head
		for (int j=0; j < block.size(); j++) {
			String[] c = block.get(j);
			int startPos = Integer.parseInt(c[5]) - 1;
			int endPos = Integer.parseInt(c[6]) - 1;
			int headPos = Integer.parseInt(c[7]) - 1;
			if (headPos == repHeadPos &&
					(startPos > repStartPos || endPos < repEndPos)) {
				repID = mentionID + j;
				rep = c;
				repStartPos = Integer.parseInt(rep[5]) - 1;
				repEndPos = Integer.parseInt(rep[6]) - 1;
				repHeadPos = Integer.parseInt(rep[7]) - 1;
			}
		}

		for (String[] c : block) {
			int sentenceID = Integer.parseInt(c[3]);
			int startPos = Integer.parseInt(c[5]) - 1;
			int endPos = Integer.parseInt(c[6]) - 1;
			int headPos = Integer.parseInt(c[7]) - 1;
			String mentionSpan = rep[9];
			String mentionType = rep[10];
			String number = rep[11].charAt(0) + "";
			String gender = rep[12].charAt(0) + "";
			String animacity = rep[13].charAt(0) + "";
			String repKey = mentionSpan +"|" + mentionType + "|"+ number + "|" + gender + "|" + animacity;

			w1.write(mentionID + "\t" + sentenceID + "\t" + startPos + "\t" + endPos + "\t" + headPos + "\t" +
					repID + "\t" + mentionType + "\t" + number + "\t" + gender + "\t" + animacity + "\n"); //"\t" + repKey + "\n"); //mentionSpan + "\t" + mentionType + "\t" + number + "\t" + gender + "\t" + animacity + "\n");

			if (mentionType.equals("PROPER"))
				w2.write(mentionID + "\t" + sentenceID + "\t" + headPos + "\t" + repKey + "\n");

			mentionID++;
		}

		//chain++;
	}
	*/

	// Plan:
	//   1. prefer mention spans with NNP (and maybe IN inside)
	//   2. prefer mention spans with overlap with Stanford NER
	//   3. try increasing recall by allowing all propers/ all great propers as names

	static int mentionID = 0;

	private static void plan1() throws IOException
	{
		// assumes that sentences.articleIDs and sentences.const have exactly the same number of lines
		BufferedWriter w1 = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(out1)));
		BufferedWriter w2 = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(out2)));
		BufferedWriter w3 = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(out3)));
		BufferedReader r1 = new BufferedReader(new InputStreamReader(new FileInputStream(dir + "/sentences.articleIDs")));
		BufferedReader r2 = new BufferedReader(new InputStreamReader(new FileInputStream(dir + "/sentences.const")));
		BufferedReader r3 = new BufferedReader(new InputStreamReader(new FileInputStream(dir + "/sentences.stanfordcoref")));
		String lastCorefArticle = "";
		String lastCorefChain = "";
		List<Line> lastCorefBlock = new ArrayList<Line>();

		String l1 = r1.readLine(), l2 = r2.readLine(), l3 = null;
		// some sentences could be skipped in r2 compared to r1
		while (l1 != null && l2 != null && Integer.parseInt(l2.split("\t")[0]) > Integer.parseInt(l1.split("\t")[0])) l1 = r1.readLine();

		int lastArticleID = -1;
		// sentenceID ==> tags (
		Map<Integer,String[]> lastArticleSentences = new HashMap<Integer,String[]>();
		while ((l3 = r3.readLine())!= null) {
			String[] c3 = l3.split("\t");
			int articleID = Integer.parseInt(c3[0]);
			if (articleID != lastArticleID) {
				lastArticleSentences.clear();
				while (l1 != null && l2 != null && Integer.parseInt(l1.split("\t")[1]) < articleID) {
					l1 = r1.readLine(); l2 = r2.readLine();
					// some sentences could be skipped in r2 compared to r1
					while (l1 != null && l2 != null && Integer.parseInt(l2.split("\t")[0]) > Integer.parseInt(l1.split("\t")[0])) l1 = r1.readLine();
				}
				while (l1 != null && l2 != null && Integer.parseInt(l1.split("\t")[1]) == articleID) {
					String[] c2 = l2.split("\t");
					int sentenceID = Integer.parseInt(c2[0]);
					String[] tags = new String[200];
					if (c2.length > 1) {
						String[] ts = c2[1].split("\\|");
						for (String t : ts) {
							//System.out.println(t);
							String[] ss = t.split(" ");
							String tag = ss[0];
							int start = Integer.parseInt(ss[1]);
							int end = Integer.parseInt(ss[2]);
							if (start >= 0 && start == end && (tag.equals("DT") || tag.equals("NNP") || tag.equals("NNPS") || tag.equals("IN")))
								tags[start] = tag;
						}
					}
					lastArticleSentences.put(sentenceID, tags);
					l1 = r1.readLine(); l2 = r2.readLine();
					// some sentences could be skipped in r2 compared to r1
					while (l1 != null && l2 != null && Integer.parseInt(l2.split("\t")[0]) > Integer.parseInt(l1.split("\t")[0])) l1 = r1.readLine();
				}
				lastArticleID = articleID;
			}
			//System.out.print("article " + articleID + ": " + lastArticleSentences.size() + " sentences: ");
			//for (int id : lastArticleSentences.keySet())
			//	System.out.print(id + " " );
			//System.out.println();


			// augment and blockify
			// here we have all article sentences and can process them
			if (!c3[0].equals(lastCorefArticle) || !c3[1].equals(lastCorefChain)) {
				process(lastCorefBlock, w1, w2, w3);
				lastCorefBlock.clear();
				//System.out.println("----------");
			}
			lastCorefArticle = c3[0];
			lastCorefChain = c3[1];

			String[] tags = lastArticleSentences.get(Integer.parseInt(c3[3]));
			//System.out.println("looking up sentence " + c3[3] + " + (article " + c3[0] + "): " + (tags) );
			int startPos = Integer.parseInt(c3[5]) - 1;
			int endPos = Integer.parseInt(c3[6]) - 1;
			boolean satisfied = true;
			for (int i=startPos; i < endPos; i++)
				if (tags[i] == null) satisfied = false;

			// want to drop determiner at beginning and possessive 's at end
			String name = c3[9];
			if (name.startsWith("the ")) name = name.substring(4);
			if (startPos == 0 && name.startsWith("The ")) name = name.substring(4);
			if (name.endsWith(" 's")) name = name.substring(0, name.length()-3);


			//if (c3[14].equals("true"))
			//	System.out.print("*** ");
			//if (satisfied)
			//	System.out.print("GREAT ");
			//if (c3[10].equals("PROPER"))
			//	System.out.print("PROPER ");
			//System.out.println(name);

			// write single best mentionSpan for block: GREAT PROPER, then PROPER, ...
			Line l = new Line();
			l.c3 = c3;
			l.name = name;
			l.great = satisfied;
			l.proper = c3[10].equals("PROPER");
			l.startPos = Integer.parseInt(c3[5]) - 1;
			l.endPos = Integer.parseInt(c3[6]) - 1;
			l.headPos = Integer.parseInt(c3[7]) - 1;
			l.sentenceID = Integer.parseInt(c3[3]);
			l.mentionSpan = c3[9];
			l.mentionType = c3[10];
			l.number = c3[11]; //.charAt(0) + "";
			l.gender = c3[12]; //.charAt(0) + "";
			l.animacity = c3[13]; //.charAt(0) + "";
			lastCorefBlock.add(l);
		}
		process(lastCorefBlock, w1, w2, w3);
		w1.close();
		w2.close();
		w3.close();


	}

	static class Line {
		String[] c3;
		boolean great;
		boolean proper;
		String name;

		int mentionID;
		int startPos;
		int endPos;
		int headPos;
		int sentenceID;
		String mentionSpan;
		String mentionType;
		String number;
		String gender;
		String animacity;
	}

	//static HashSet<Integer> repWritten = new HashSet<Integer>();

	static void process(List<Line> block, BufferedWriter w1,
			BufferedWriter w2, BufferedWriter w3) throws IOException {
		if (block.isEmpty()) return;

		Line rep = null;
		for (int j=0; j < block.size(); j++) {
			Line cur = block.get(j);
			cur.mentionID = mentionID + j;
			if (rep == null ||
				(!rep.proper && cur.proper) ||
				(!rep.great && cur.great)) {
				// find shortest mention with same head
				//if (headPos == repHeadPos &&
				//		(startPos > repStartPos || endPos < repEndPos)) {
				// if it is not proper, ignore
				//if (!representativeMention[10].equals("PROPER")) return;
				rep = cur;
			}
		}

		//String repKey = rep.name +"|" + rep.mentionType + "|"+
		//	rep.number.charAt(0) + "|" + rep.gender.charAt(0) + "|" + rep.animacity.charAt(0);
		String repKey = rep.name;


		HashSet<String> hs = new HashSet<String>();
		for (Line c : block) {
			w1.write(mentionID + "\t" + c.sentenceID + "\t" + c.headPos + "\t" + c.startPos + "\t" + c.endPos + "\t" +
					rep.mentionID + "\t" + c.mentionType + "\t" + c.number + "\t" + c.gender + "\t" + c.animacity + "\n"); //"\t" + repKey + "\n"); //mentionSpan + "\t" + mentionType + "\t" + number + "\t" + gender + "\t" + animacity + "\n");

			String l = c.sentenceID + "\t" + c.headPos + "\t" + repKey + "\n";
			if (rep.mentionType.equals("PROPER") && !hs.contains(l)) {
				w2.write(l);
				hs.add(l);
			}

			mentionID++;
		}

		if (rep.mentionType.equals("PROPER"))
			w3.write(rep.mentionID + "\t" + rep.name + "\n");

		//chain++;

	}

}

