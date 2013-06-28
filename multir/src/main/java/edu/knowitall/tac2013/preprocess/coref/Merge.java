package edu.knowitall.tac2013.preprocess.coref;

import java.io.IOException;

public class Merge {

	public static void main(String[] args) throws IOException {

		for (int i=0; i < 45; i++)
			//System.out.println("cat sentences.stanfordcoref." + i + ">> ../sentences.stanfordcoref");
			//System.out.println("cat sentences.stanfordner." + i + ">> ../sentences.stanfordner");
			//System.out.println("cat sentences.stanfordpos." + i + ">> ../sentences.stanfordpos");
			System.out.println("cat sentences.stanfordlemma." + i + ">> ../sentences.stanfordlemma");
	}
}
