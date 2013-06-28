package edu.knowitall.tac2013.preprocess.tac;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.HashMap;

public class CreateArticleIDs {

	static String dir = "/projects/pardosa/s5/raphaelh/tac/data/09nw";

	public static void main(String[] args) throws IOException {
		BufferedReader r1 = new BufferedReader(new InputStreamReader
				(new FileInputStream(dir + "/sentences.meta"), "utf-8"));
		BufferedWriter w = new BufferedWriter(new OutputStreamWriter
				(new FileOutputStream(dir + "/sentences.articleIDs"), "utf-8"));
		String l = null;
		HashMap<String,Integer> m = new HashMap<String,Integer>();
		while ((l = r1.readLine())!= null) {
			String[] t = l.split("\t");
			Integer i = m.get(t[2]);
			if (i == null) {
				i = m.size();
			}
			m.put(t[2], i);
			w.write(t[0] + "\t" + i + "\n");
		}
		r1.close();
		w.close();
	}

}
