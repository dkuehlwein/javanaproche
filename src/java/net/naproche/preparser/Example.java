package net.naproche.preparser;
import java.util.Hashtable;
import java.util.LinkedList;
import jpl.Query;
import jpl.*;

// Example: Calls create_naproche_input from Java and parses the returnvalue with convertSentences.

public class Example{
	public static void main(String args[]){
		new Query("['src/prolog/load_jpl'].").oneSolution();
		Query create_naproche_input =
			// new Query("create_naproche_input('Let $n$ be in $\\\\mathbb{N}$. Then $n > 0$.',L).");
			// new Query("create_naproche_input('\\\\begin{Proof} trivial. \\\\end{Proof}',L).");
			new Query("create_naproche_input('test',L).");

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
