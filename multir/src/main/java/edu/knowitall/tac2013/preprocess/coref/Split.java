package edu.knowitall.tac2013.preprocess.coref;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;

// RE-RUN THE SPLIT, TO FIX last partition

public class Split {

	// split should be document-aware, so that sentences for same document are always together


	static String inDir = "/projects/pardosa/data15/raphaelh/data";
	static String outDir = "/projects/pardosa/data15/raphaelh/data/tmp2";

	static String[] files = { "sentences.articleIDs", "sentences.cj",
		"sentences.text", "sentences.tokens", "sentences.tokenSpans" };

	static int lines = 1000000;

	public static void main(String[] args) throws IOException {

		// first split sentences.articleIDs
		int partition = 0;
		{
			BufferedWriter w = new BufferedWriter(new OutputStreamWriter
					(new FileOutputStream(outDir + "/" + files[0] + "." + partition), "utf-8"));
			int count = 0;
			int lastArticleID = -1;
			BufferedReader r = new BufferedReader(new InputStreamReader
					(new FileInputStream(inDir + "/" + files[0]), "utf-8"));
			String l = null;
			while ((l = r.readLine())!= null) {
				String[] c = l.split("\t");
				int articleID = Integer.parseInt(c[1]);
				if (articleID != lastArticleID && count >= lines) {
					w.close();
					partition++;
					w = new BufferedWriter(new OutputStreamWriter
							(new FileOutputStream(outDir + "/" + files[0] + "." + partition), "utf-8"));
					count = 0;
				}
				w.write(l);
				w.write('\n');
				count++;
				lastArticleID = articleID;
			}
			r.close();
			w.close();
		}
		partition++;

		// determine splitIDs
		int[] startIDs = determineStartIDs(partition);

		// split other files based on these partitions
		split(files[1], startIDs);
		split(files[2], startIDs);
		split(files[3], startIDs);
		split(files[4], startIDs);
	}

	static int[] determineStartIDs(int partitions) throws IOException {
		int[] ps = new int[partitions];
		for (int p = 0; p < partitions; p++) {
			BufferedReader r = new BufferedReader(new InputStreamReader
					(new FileInputStream(outDir + "/" + files[0] + "." + p), "utf-8"));
			String l = r.readLine();
			String[] c = l.split("\t");
			int sentenceID = Integer.parseInt(c[0]);
			r.close();
			ps[p] = sentenceID;
		}
		return ps;
	}


	static void split(String file, int[] startIDs) throws IOException {
		BufferedReader r = new BufferedReader(new InputStreamReader
				(new FileInputStream(inDir + "/" + file), "utf-8"));
		String l = r.readLine();

		for (int p = 0; p < startIDs.length; p++) {
			int maxID = (p+1 < startIDs.length)? startIDs[p+1] : Integer.MAX_VALUE;
			BufferedWriter w = new BufferedWriter(new OutputStreamWriter
					(new FileOutputStream(outDir + "/" + file + "." + p), "utf-8"));
			do {
				String[] c = l.split("\t");
				int sentenceID = Integer.parseInt(c[0]);

				if (sentenceID < maxID) {
					w.write(l);
					w.write('\n');
				} else {
					break;
				}
			} while ((l = r.readLine()) != null);
			w.close();
		}
		r.close();
	}
}
