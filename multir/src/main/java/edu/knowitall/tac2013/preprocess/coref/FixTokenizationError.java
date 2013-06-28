package edu.knowitall.tac2013.preprocess.coref;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;

public class FixTokenizationError {

	static final String dir = "/projects/pardosa/data15/raphaelh/data";
	static final String in1 = dir + "/sentences.tokens";
	static final String out1 = dir + "/sentences.tokens2";

	public static void main(String[] args) throws Exception {

		BufferedWriter w1 = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(out1), "utf-8"));
		BufferedReader r1 = new BufferedReader(new InputStreamReader(new FileInputStream(in1), "utf-8"));
		String l = null;
		while ((l = r1.readLine())!= null) {
			String[] c= l.split("\t");
			w1.write(c[0] + "\t");
			String[] t = c[1].split(" ");
			for (int i=0; i < t.length; i++) {
				String tok = t[i];
				if (tok.length() == 0) continue;
				if (i > 0) w1.write(" ");
				w1.write(tok);
			}
			w1.write("\n");
		}
		r1.close();
		w1.close();
	}
}
