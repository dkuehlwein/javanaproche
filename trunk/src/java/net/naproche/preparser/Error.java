package net.naproche.preparser;
import java.util.LinkedList;

//Containerclass for one Error

public class Error{
	// Each error has a type (ie "inputError"), a syntactical position (ie "sentence"), start and end (position of leading and tailing character), content (Atom which triggered the error) and description (ie "Could not parse input.")
	String type;
	String position;
	int start;
	int end;
	String content;
	String description;

	public Error(String inString){
		// Strip "message(error, " and split on ,:
		String[] split = inString.substring(15).split(",");

		// trim everything (lazy..) and cut of various irrelevant characters (like ' or ,) with substring.
		this.type = split[0].trim();
		this.position = split[1].trim().substring(1).trim();
		this.start = Integer.valueOf(split[2].trim());
		this.end = Integer.valueOf(split[3].trim().substring(0,split[3].trim().length()-1).trim());
		this.content = split[4].trim();
		this.description = split[5].trim().substring(1,split[5].trim().length()-3);
	// Note: Minimal changes in how add_error_message_once forms the String may cause this parsing to malfunction, since it relies partially on absolute indices. Also a "," in any field would break it.
	}

	// OUT: A String which reads exactly like the error in swipl
	public String toString(){
		String retVal="message(error, ";
		retVal = retVal + type+", '"+position+", "+start+", "+end+"', "+content+", '"+description+"')";
		return retVal;

	}

	// IN: String returned by get_messages/PROLOG
	// OUT: LinkedList containing the errors.
	// DEBUG: Utility-Method, may be moved
	public static LinkedList<Error> convertErrors(String inString){
		LinkedList<String> temp = Sentence.convertDotNotation(inString);
		LinkedList<Error> retVal = new LinkedList<Error>();
		for (String error : temp)
			retVal.add(new Error(error));
		return retVal;
	}
}
