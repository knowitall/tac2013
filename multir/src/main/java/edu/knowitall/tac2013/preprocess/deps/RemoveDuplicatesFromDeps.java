package edu.knowitall.tac2013.preprocess.deps;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.HashSet;
import java.util.Set;

public class RemoveDuplicatesFromDeps {

	//static String in = "/projects/pardosa/data15/raphaelh/data/sentences.depsStanfordCCProcessed";
	//static String out = "/projects/pardosa/data15/raphaelh/data/sentences.depsStanfordCCProcessed.nodup";

	//static String in = "/projects/pardosa/data15/raphaelh/roth-data/sentences.depsStanfordCCProcessed";
	//static String out = "/projects/pardosa/data15/raphaelh/roth-data/sentences.depsStanfordCCProcessed.nodup";

	//static String in = "/projects/pardosa/data16/raphaelh/patterns/data/sentences.depsStanfordCCProcessed";
	//static String out = "/projects/pardosa/data16/raphaelh/patterns/data/sentences.depsStanfordCCProcessed.nodup";

	//static String in = "/projects/pardosa/s5/raphaelh/tac/data/09nw/sentences.depsStanfordCCProcessed";
	//static String out = "/projects/pardosa/s5/raphaelh/tac/data/09nw/sentences.depsStanfordCCProcessed.nodup";

	//static String in = "/projects/pardosa/data14/raphaelh/data/ace05/sentences.depsStanfordCCProcessed";
	//static String out = "/projects/pardosa/data14/raphaelh/data/ace05/sentences.depsStanfordCCProcessed.nodup";

	//static String in = "/projects/pardosa/data15/raphaelh/wex/tmp/sentences.depsStanfordCCProcessed";
	//static String out = "/projects/pardosa/data15/raphaelh/wex/tmp/sentences.depsStanfordCCProcessed.nodup";

	//static String in = "/projects/pardosa/data15/raphaelh/roth-data/sentences.kbest.depsStanfordCCProcessed";
	//static String out = "/projects/pardosa/data15/raphaelh/roth-data/sentences.kbest.depsStanfordCCProcessed.nodup";

	static String in = "/projects/pardosa/data15/raphaelh/biology/sentences.depsStanfordCCProcessed";
	static String out = "/projects/pardosa/data15/raphaelh/biology/sentences.depsStanfordCCProcessed.nodup";


	// kbest or single best
	static boolean KBEST = false; //true;

	public static void main(String[] args) throws IOException {

		BufferedReader r = new BufferedReader(new InputStreamReader
				(new FileInputStream(in), "utf-8"));
		BufferedWriter w = new BufferedWriter(new OutputStreamWriter
				(new FileOutputStream(out), "utf-8"));
		String l = null;
		while ((l = r.readLine())!= null) {
			String[] t = l.split("\t");
			if ((KBEST && t.length != 3) ||
					(!KBEST && t.length != 2)) {
				w.write(l + "\n");
				continue;
			}
			String[] d = t[KBEST? 2 : 1].split("\\|");
			Set<String> hs = new HashSet<String>();
			for (int i=0; i < d.length; i++) hs.add(d[i]);

			w.write(t[0] + "\t");
			if (KBEST) w.write(t[1] + "\t");
			boolean first = true;
			for (String s : hs) {
				if (!first) w.write("|");
				w.write(s);
				first = false;
			}
			w.write("\n");
		}
		r.close();
		w.close();
	}

}
