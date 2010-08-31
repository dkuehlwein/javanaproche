import java.util.LinkedList;

//Containerclass for one word
public class Word{
	// Start and End of the word (absolute values in respect of the whole text)
	public int start, end;

	// either "word" or "math"
	String type;

	// empty when type=="math"
	String wordContent;

	// empty when type=="word"
	LinkedList<String> mathContent;

	// inString is either the representation of a PROLOG-list containing the math-elements or the word itself
	public Word(String start, String end, String type, String inString){
		this.start = Integer.valueOf(start);
		this.end = Integer.valueOf(end);
		this.type = type;
		if (type.equals("math")){
			mathContent = Sentence.convertDotNotation(inString.substring(5,inString.length()-1)); //Strip "math(" and ")"
			wordContent = "";
		}
		else{
			wordContent=inString;
			mathContent=new LinkedList<String>();
		}
	}

	// IN: List containing String-representations of PROLOG-predicates containing type, start and end of words
	// OUT: List containing triples (implemented as LinkedLists) containing type, start and end of words
	// DEBUG: Utility-Method, may be moved
	public static LinkedList<LinkedList<String>> convertWord(LinkedList<String> inList){
		LinkedList<LinkedList<String>> retVal = new LinkedList<LinkedList<String>>();
		String[] splitspace = new String[3];
		for (String word : inList){
			LinkedList<String> tmp = new LinkedList<String>();
			// Assumption: types have exactly 4 characters (currently true as there are only "math" and "word")
			splitspace = word.substring(5,word.length()-1).split(",");
			if (word.startsWith("word"))
				tmp.add("word");
			else if (word.startsWith("math"))
				tmp.add("math");
			tmp.add(splitspace[0].trim());
			tmp.add(splitspace[1].trim());
			retVal.add(tmp);
		}
		return retVal;
	}

	// OUT: String containing the content of the word. (NOT type, start or end)
	// Not to be called directly, is implicitly called by Sentence.toString()
	public String toString(){
		String retVal="";
		if (type == "word")
			retVal = wordContent;
		else if (type == "math")
			retVal = "math("+mathContent.toString()+")";
		return retVal;
	}

}
