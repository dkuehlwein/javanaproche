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
		String[] arr = new String[5], split = new String[3];
		int arr_index=0, str_index=0, pairs=0;

		for (int i=0; i<inString.length(); i++){
			if (inString.substring(i,i+1).equals("'") && pairs==0)
				pairs++;
			else if (inString.substring(i,i+1).equals("'") && pairs==1)
				pairs--;
			else if (inString.substring(i,i+1).equals(",") && pairs==0){
				arr[arr_index]=inString.substring(str_index+2,i);
				str_index=i;
				arr_index++;
			}
		}
		arr[arr_index] = inString.substring(str_index+2,inString.length());

		split = arr[2].split(", ");

		// trim everything (I'm lazy..) and cut of various irrelevant characters (like ' or ,) with substring.
		this.type = arr[1].trim();
		this.position = split[0].trim().substring(1);
		this.start = Integer.valueOf(split[1].trim());
		this.end = Integer.valueOf(split[2].substring(0,split[2].length()-1).trim());
		this.content = arr[3].trim().substring(1,arr[3].trim().length()-1);
		this.description = arr[4].trim().substring(1,arr[4].trim().length()-2);
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
