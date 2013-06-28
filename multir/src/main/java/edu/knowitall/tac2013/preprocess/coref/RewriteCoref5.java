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

public class RewriteCoref5 {

//	static String in = "/projects/pardosa/data15/raphaelh/readr2exp/nythalfd_dev/sentences.stanfordcoref";
//	static String out1 = "/projects/pardosa/data15/raphaelh/readr2exp/nythalfd_dev/sentences.stanfordcoref.all";
//	static String out2 = "/projects/pardosa/data15/raphaelh/readr2exp/nythalfd_dev/sentences.stanfordcoref.proper";

//	static String in = "/projects/pardosa/data15/raphaelh/readr2exp/nythalfd_test/sentences.stanfordcoref";
//	static String out1 = "/projects/pardosa/data15/raphaelh/readr2exp/nythalfd_test/sentences.stanfordcoref.all";
//	static String out2 = "/projects/pardosa/data15/raphaelh/readr2exp/nythalfd_test/sentences.stanfordcoref.proper";

	static String in = "/projects/pardosa/data15/raphaelh/biology/sentences.stanfordcoref";
	static String out1 = "/projects/pardosa/data15/raphaelh/biology/sentences.stanfordcoref.all";
	static String out2 = "/projects/pardosa/data15/raphaelh/biology/sentences.stanfordcoref.proper";

	// COLUMNS: mentionID, sentenceID, headPos, repKey

	public static void main(String[] args) throws IOException {
		// read blocks of coreferences (chains) and determine the most representative of each

		BufferedWriter w1 = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(out1)));
		BufferedWriter w2 = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(out2)));
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
				process(block, w1, w2);
				block.clear();
				block.add(c);
			}
		}
		if (!block.isEmpty())
			process(block, w1, w2);
		r.close();
		w1.close();
		w2.close();
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
}

