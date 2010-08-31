import java.util.LinkedList;

public class Word{
	public int start, end;
	String type;
	String wordContent;
	LinkedList<String> mathContent;

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

	public static LinkedList<LinkedList<String>> convertWord(LinkedList<String> inList){
		LinkedList<LinkedList<String>> retVal = new LinkedList<LinkedList<String>>();
		String[] splitspace = new String[3];
		for (String word : inList){
			LinkedList<String> tmp = new LinkedList<String>();
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

	public String toString(){
		// String retVal="Type: "+type+", Start: "+Start+", End: "+End;
		String retVal="";
		if (type == "word")
			retVal = wordContent;
		else if (type == "math")
			retVal = "math("+mathContent.toString()+")";
		return retVal;
	}

}
