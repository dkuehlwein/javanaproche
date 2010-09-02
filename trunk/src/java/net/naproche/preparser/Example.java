package net.naproche.preparser;
import java.util.Hashtable;
import java.util.LinkedList;
import java.util.*;
import jpl.Query;
import jpl.*;
import java.io.*;

// Example: Calls create_naproche_input from Java and parses the returnvalue with convertSentences.

public class Example{
	public static void main(String args[]){
		
		/*
		String dir="user.dir"; // set to current directory
		 try {dir=new File(System.getProperty(dir)).getCanonicalPath();}
		 catch (IOException e1) { 
		 //handler required but null  
		}
		 System.out.println ("Current dir : " + dir);
		 
		 File test = new File("examples/mizar");
		 String[] files = test.list();
		 
		 List scrambled = Arrays.asList(files);
		 //Collections.sort(scrambled, new NaturalOrderComparator());
		 
		 System.out.println("Scrambled: " + scrambled);
		 
		 for (String file: files) {
		         System.out.println(file);
		 }
		//*/
		new Query("['src/prolog/load_jpl'].").oneSolution();
		Query create_naproche_input =
			// new Query("create_naproche_input('Let $n$ be in $\\\\mathbb{N}$. Then $n > 0$.',L).");
			// new Query("create_naproche_input('\\\\begin{Proof} trivial. \\\\end{Proof}',L).");
			// new Query("create_naproche_input('test',L).");
			new Query("create_naproche_input('test. $\\\\frac{a}{b}$.',L).");

		String output;
		try{
			output = create_naproche_input.oneSolution().get("L").toString();
			LinkedList<Sentence> ll = Sentence.convertSentences(output);
			System.out.println(ll);
		}
		catch (java.lang.NullPointerException ex){
			Query get_messages = new Query("get_messages(Messages)");
			output = get_messages.oneSolution().get("Messages").toString();
			LinkedList<Error> ll = Error.convertErrors(output);
			System.out.println(ll);
		}


	}
}
