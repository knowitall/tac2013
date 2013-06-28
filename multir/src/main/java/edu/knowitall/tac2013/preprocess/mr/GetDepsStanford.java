package edu.knowitall.tac2013.preprocess.mr;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import edu.stanford.nlp.trees.GrammaticalStructure;
import edu.stanford.nlp.trees.GrammaticalStructureFactory;
import edu.stanford.nlp.trees.PennTreebankLanguagePack;
import edu.stanford.nlp.trees.Tree;
import edu.stanford.nlp.trees.TreebankLanguagePack;
import edu.stanford.nlp.trees.TypedDependency;

public class GetDepsStanford {

	static String dir = "/projects/pardosa/data16/raphaelh/patterns/data";

	static String in0 = dir + "/sentences.cj";
	static String out0 = dir + "/sentences.depsStanfordCCProcessed";

	public static void main (String[] args) throws IOException {
		in0 = args[0];
		out0 = args[1];

		TreebankLanguagePack tlp = new PennTreebankLanguagePack();
		GrammaticalStructureFactory gsf = tlp.grammaticalStructureFactory();

		BufferedReader r = new BufferedReader(new InputStreamReader
				(new FileInputStream(in0), "utf-8"));
		BufferedWriter w = new BufferedWriter(new OutputStreamWriter
				(new FileOutputStream(out0), "utf-8"));
		String line = null;

		while ((line = r.readLine())!= null) {

			String[] t = line.split("\t");

			// we don't allow | since we treat that as a special character
			t[1] = t[1].replace("|", " ");

			Tree parse = Tree.valueOf(t[1]);
			//System.out.println(parse.pennString());

			GrammaticalStructure gs = gsf.newGrammaticalStructure(parse);
			Collection<TypedDependency> tdl = null;
			try {
				tdl = /*gs.allTypedDependencies();*/ gs.typedDependenciesCCprocessed();
			} catch (NullPointerException e) {
				// there has to be a bug in EnglishGrammaticalStructure.collapseFlatMWP
				tdl = new ArrayList<TypedDependency>();
			}

			StringBuilder sb = new StringBuilder();
			List<TypedDependency> l = new ArrayList<TypedDependency>();
			l.addAll(tdl);
			for (int i=0; i < tdl.size(); i++) {
				TypedDependency td = l.get(i);
				String name = td.reln().getShortName();
				if (td.reln().getSpecific() != null)
					name += "-" + td.reln().getSpecific();
				sb.append((td.gov().index()-1) + " ");
				sb.append(name + " ");
				sb.append((td.dep().index()-1));
				if (i < tdl.size()-1)
					sb.append("|");
			}
			//System.out.println(t[0] + "\t" + sb.toString());
			w.write(t[0] + "\t" + sb.toString() + "\n");
		}

		r.close();
		w.close();
	}
}
