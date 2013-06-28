package edu.knowitall.tac2013.preprocess.tac;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;

public class CleanCJOutput {

	static String dir = "/projects/pardosa/s5/raphaelh/tac";

	static String in1 = dir + "/data/sentences.cjout";
	static String out1 = dir + "/data/sentences.cj";

	public static void main(String[] args) throws IOException {

		{
			BufferedReader r = new BufferedReader(new InputStreamReader(
					new FileInputStream(in1), "utf-8"));
			BufferedWriter w = new BufferedWriter(new OutputStreamWriter(
					new FileOutputStream(out1), "utf-8"));
			String l = null;
			while ((l = r.readLine())!= null) {
				String[] c = l.split("\t");
				w.write(c[0].trim() + "\t" + c[1].trim() + "\n");
			}
			r.close();
			w.close();
		}
	}
}
