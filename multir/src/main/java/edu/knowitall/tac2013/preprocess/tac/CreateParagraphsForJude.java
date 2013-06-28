package edu.knowitall.tac2013.preprocess.tac;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;

public class CreateParagraphsForJude {

	static String dir = "/projects/pardosa/s5/raphaelh/tac";

	static String in1 = dir + "/data/09wb/sentences.meta";
	static String in2 = dir + "/data/09wb/sentences.tokens";
	static String out = dir + "/data/09wb/paragraphs.tokens";

	public static void main(String[] args) throws IOException {

		BufferedReader r1 = new BufferedReader(new InputStreamReader
				(new FileInputStream(in1), "utf-8"));
		BufferedReader r2 = new BufferedReader(new InputStreamReader
				(new FileInputStream(in2), "utf-8"));
		BufferedWriter w = new BufferedWriter(new OutputStreamWriter
				(new FileOutputStream(out), "utf-8"));
		String l1 = r1.readLine(), l2 = r2.readLine();

		int currentPar = -1;
		StringBuilder currentTokens = new StringBuilder();

		while (l1 != null && l2 != null) {
			// they should align exactly
			String[] c1 = l1.split("\t");
			String[] c2 = l2.split("\t");
			int par = Integer.parseInt(c1[1]);
			if (currentPar != par && currentPar != -1) {
				// write existing par tokens
				w.write(currentPar + "\t" + currentTokens.toString() + "\n");
				currentTokens.setLength(0);
			}
			currentPar = par;
			if (currentTokens.length() > 0) currentTokens.append(" ");
			currentTokens.append(c2[1]);
			l1 = r1.readLine();
			l2 = r2.readLine();
		}
		if (currentPar != -1) {
			// write existing par tokens
			w.write(currentPar + "\t" + currentTokens.toString() + "\n");
			currentTokens.setLength(0);
		}
		r1.close();
		r2.close();
		w.close();
	}

}
