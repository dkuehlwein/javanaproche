package net.naproche.preparser;
import java.util.LinkedList;

// Containerclass for one sentence.

public class Sentence{
	//Each sentence has an unique id, the index of the starting character and the index of the last character (usually . or :).
	int id, start, end;

	LinkedList<Word> content;

  // inString is one substring of the returnvalue of create_naproche_input which represents one sentence
	public Sentence(String inString){
		boolean foundPos=false, foundCon=false;

		int pairs=0,posStart=0,conStart=0;

		// "pairs" is the number of open parentheses with no corresponding closing one.

		// parentheses are not to be counted if they are atoms themselfes
		// therefore they need not to be surrounded by '
		// but if they are it could be a construct like '.'('atom'
		// so this case is excluded
		for (int i=0; i<inString.length(); i++){
			if 	(inString.substring(i,i+1).equals("(")
			 	&& !(	inString.substring(i+1,i+2).equals("'")
					&&	inString.substring(i-1,i).equals("'")
					&&	!inString.substring(i-2,i-1).equals(".")
					&&	inString.substring(i-3,i-2).equals("'")
					)
				)
				pairs++;
			else if	(inString.substring(i,i+1).equals(")")
			 	&& !(	inString.substring(i-1,i).equals("'")
					&&	inString.substring(i+1,i+2).equals("'")
					&&	!inString.substring(i-2,i-1).equals(".")
					&&	inString.substring(i-3,i-2).equals("'")
					)
				)
				pairs--;
			// A list is found (starts with '.'), the only open parenthesis is the one in "sentence(" and the position of the List containing the wordpositions is not found yet. -> The list containing the wordpositions is found.
			else if(inString.substring(i,i+1).equals(".")	&&
				inString.substring(i-1,i).equals("'")	&&
				inString.substring(i+1,i+2).equals("'") &&
				pairs == 1				&&
				foundPos == false			){
					foundPos = true;
					posStart = i-1;
				}
			// A list is found (starts with '.'), the only open parenthesis is the one in "sentence(" and the position of the list containing the word-positions is already found. -> It must be the list containing the wordcontent.
			else if(inString.substring(i,i+1).equals(".")	&&
				inString.substring(i-1,i).equals("'")	&&
				inString.substring(i+1,i+2).equals("'") &&
				pairs == 1				&&
				foundCon == false			&&
				foundPos == true			){
					foundCon = true;
					conStart = i-1;
				}
		}

		// empty sentence case.
		if(posStart==0 || conStart==0){
			posStart = inString.split("\\[\\]")[0].length(); // number of characters until [] (position list)
			conStart = inString.split("\\[\\]")[1].length()+posStart+2; // "[]" is not incluced in split, so add 2
		}

		// first 9 characters are "sentence(", following are id, start and end, seperated by ,.
		String[] idStartEnd = inString.substring(9,posStart).split(",");

		this.id = Integer.parseInt(idStartEnd[0].trim());
		this.start = Integer.parseInt(idStartEnd[1].trim());
		this.end = Integer.parseInt(idStartEnd[2].trim());

		// Strings representing the lists containting wordpositions and wordcontent
		String pos = inString.substring(posStart,conStart-1);
		String con = inString.substring(conStart);

		//Lists containing the strings representing the wordpositions and wordcontent
		LinkedList<String> tmpPositions = convertDotNotation(pos);
		LinkedList<String> tmpContent = convertDotNotation(con);

		//List containing triples containing [type, start, end] for each word.
		LinkedList<LinkedList<String>> positions = Word.convertWord(tmpPositions);

		content = new LinkedList<Word>();
		Word tmpWord;

		// Constructing individual words.
		// Assumption: Elements in positions and elements in tmpContent are exactly bijective
		for(int i = 0; i < positions.toArray().length; i++){
			tmpWord = new Word(positions.get(i).get(1),
					positions.get(i).get(2),
					positions.get(i).get(0),
					tmpContent.get(i));
			content.add(tmpWord);
		}
	};

	// OUT: A String which reads exactly like the sentence in the swipl interpreter
	public String toString(){
		String retVal = "sentence(";
		retVal = retVal+id+", "+start+", "+end+", ";
		retVal = retVal.concat("[");
		for (Word p : this.content){
			retVal = retVal.concat(p.type);
			retVal = retVal.concat("(");
			retVal = retVal.concat(String.valueOf(p.start));
			retVal = retVal.concat(", ");
			retVal = retVal.concat(String.valueOf(p.end));
			retVal = retVal.concat(")");
			retVal = retVal.concat(", ");
		}
		retVal = retVal.substring(0,retVal.length()-2);
		retVal = retVal.concat("], ");
		retVal = retVal.concat(content.toString());
		retVal = retVal.concat(")");
		return retVal;
	}

	// Constructs a LinkedList from a String representing a PROLOG-list in recursive .-Notation.
	// Does not recurse into nested lists.
	// IN: String (or Substring) returned by PROLOG which represents a list, starting with '.'
	// OUT: LinkedList containing the Elements of said list
	// DEBUG: Utility-Method, may be moved
	public static LinkedList<String> convertDotNotation(String inString){
		int l = inString.length();
		int pairs = 0;
		int j=l;
		if (l < 3 || !inString.substring(0,3).equals("'.'")){
			return new LinkedList<String>();
		}
		// comma on parentheses-level 1 indicates a new member of the list.
		for (int i = 0; i<l; i++){
			// parentheses are not to be counted if they are atoms themselfes
			// therefore they need not to be surrounded by '
			// but if they are it could be a construct like '.'('atom'
			// so this case is excluded
			if (inString.substring(i,i+1).equals( "(" )
			 	&& !(	inString.substring(i+1,i+2).equals("'")
					&&	inString.substring(i-1,i).equals("'")
					&&	!inString.substring(i-2,i-1).equals(".")
					&&	inString.substring(i-3,i-2).equals("'")
					)
				)
					pairs++;
			else if (inString.substring(i,i+1).equals( ")" )
			 	&& !(	inString.substring(i-1,i).equals("'")
					&&	inString.substring(i+1,i+2).equals("'")
					&&	!inString.substring(i-2,i-1).equals(".")
					&&	inString.substring(i-3,i-2).equals("'")
					)
				)
					pairs--;
			// ',' could be an atom, so this case is excluded.
			// Also commas which seperate members always have a following space
			else if (inString.substring(i,i+1).equals( "," )
			 	&& !(	inString.substring(i-1,i).equals("'")
					&&	inString.substring(i+1,i+2).equals("'")
					)
				&&	inString.substring(i+1,i+2).equals(" ")
				&& pairs == 1){
					j = i+2;
					break;
				}
			}
		// recurses on the tail of the string after a comma is found
		LinkedList<String> retVal = new LinkedList<String>(convertDotNotation(inString.substring(j,l-1)));
		// adding found element (before the comma)
		retVal.addFirst(inString.substring(4,j-2));
		return retVal;
	}

	// IN: String returned by create_naproche_input/PROLOG
	// OUT: LinkedList containing the sentences.
	// DEBUG: Utility-Method, may be moved
	public static LinkedList<Sentence> convertSentences(String inString){
		LinkedList<String> temp = convertDotNotation(inString);
		LinkedList<Sentence> retVal = new LinkedList<Sentence>();
		for (String sentence : temp)
			retVal.add(new Sentence(sentence));
		return retVal;
	}
}
