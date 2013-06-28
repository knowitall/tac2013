package edu.knowitall.tac2013.preprocess.tac;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;

//There seems to be a bug in the CJ parser:
//if a sentence starts with "@" then the parser fails.

public class PrepareSentencesForCJ {

	static String dir =  "/projects/pardosa/s5/TACKBP";
	//"/projects/pardosa/s5/raphaelh/tac";

	// remove @

	static String input = dir + "/data/10wb/sentences.tokens";
	static String output = dir + "/data/10wb/sentences.cjin";

	//static String input = dir + "/../wex/sentences.tokens";
	//static String output = dir + "/../wex/sentences.cjin";

	public static void main(String[] args) throws IOException {

		BufferedReader r = new BufferedReader(new InputStreamReader
				(new FileInputStream(input), "utf-8"));
		BufferedWriter w = new BufferedWriter(new OutputStreamWriter
				(new FileOutputStream(output), "utf-8"));
		String line = null;
		while ((line = r.readLine())!= null) {
			line = line.replace("\t@", "\t" + "AT");

			// cj has a limit of 256 tokens
			String[] c = line.split("\t");
			if (c.length < 2) continue;
			String[] toks = c[1].split(" ");
			if (toks.length > 120) {
				continue;
				//c[1] = " ";
			}
			w.write(c[0] + "\t" + c[1] + "\n");
		}
		r.close();
		w.close();
	}

}
