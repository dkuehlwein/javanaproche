import java.util.Hashtable;
import java.util.LinkedList;
import jpl.Query;
import jpl.*;

// Example: Calls create_naproche_input from Java and parses the returnvalue with convertSentences.

public class Javatest{
	public static void main(String args[]){
		new Query("[load].").oneSolution();
		Query create_naproche_input =
			// new Query("create_naproche_input('Let $n$ be in $\\\\mathbb{N}$. Then $n > 0$.',L).");
			 new Query("create_naproche_input('\\\\begin{Proof} trivial. \\\\end{Proof}',L).");
		String output = create_naproche_input.oneSolution().get("L").toString();
		System.out.println(output);

		LinkedList<Sentence> ll = Sentence.convertSentences(output);

		System.out.println(ll);
	}
}
