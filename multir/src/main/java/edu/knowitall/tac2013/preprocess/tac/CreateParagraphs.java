package edu.knowitall.tac2013.preprocess.tac;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintStream;

public class CreateParagraphs {

	private PrintStream metaOutput;
	private PrintStream textOutput;
	private BufferedReader sentencesInput;
	
	public CreateParagraphs(BufferedReader sentencesInput, PrintStream metaOutput, PrintStream textOutput) {
		
		this.sentencesInput = sentencesInput;
		this.textOutput = textOutput;
		this.metaOutput = metaOutput;
		
	}

	public void createParagraphs() throws IOException {

		String l = null;
		int i = 0;
		while ((l = sentencesInput.readLine())!= null) {
			String[] c = l.split("\t");
			if (c.length < 5) {System.out.println(l); continue; }
			metaOutput.print(c[0] + "\t" + i + "\t" + c[2] + "\n");
			textOutput.print(i + "\t" + c[0] + "\t" + /*c[1] + "\t" +*/ c[3] + "\t" + c[4] + "\n");
			i++;
		}
	}

	public void close() throws IOException {
		sentencesInput.close();
		textOutput.close();
		metaOutput.close();
	}
	
	public static void main(String[] args) throws IOException {
	
		String inputFile = args[0];
		String textOutputFile = args[1];
		String metaOutputFile = args[2];
		
		BufferedReader rawInput;
		if (inputFile.equals("stdin")) rawInput = new BufferedReader(new InputStreamReader(System.in));
		else rawInput = new BufferedReader(new FileReader(inputFile));
		
		PrintStream textOutput;
		if (textOutputFile.equals("stdout")) textOutput = System.out;
		else textOutput = new PrintStream(textOutputFile);
		
		PrintStream metaOutput = new PrintStream(metaOutputFile);
		
		CreateParagraphs cp = new CreateParagraphs(rawInput, textOutput, metaOutput);
		
		cp.createParagraphs();
		
		cp.close();
	}
}
