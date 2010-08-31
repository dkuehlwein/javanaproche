import java.util.LinkedList;

public class Sentence{
	int id, start, end;
	LinkedList<Word> content;

	public Sentence(String inString){
		boolean foundPos=false, foundCon=false;

		int pairs=0,posStart=0,conStart=0;

		for (int i=0; i<inString.length(); i++){
			if 	(inString.substring(i,i+1).equals("(")	)
				pairs++;
			else if	(inString.substring(i,i+1).equals(")")	)
				pairs--;
			else if(inString.substring(i,i+1).equals(".")	&&
				inString.substring(i-1,i).equals("'")	&&
				inString.substring(i+1,i+2).equals("'") &&
				pairs == 1				&&
				foundPos == false			){
					foundPos = true;
					posStart = i-1;
				}
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

		String[] idStartEnd = inString.substring(9,posStart).split(",");

		this.id = Integer.parseInt(idStartEnd[0].trim());
		this.start = Integer.parseInt(idStartEnd[1].trim());
		this.end = Integer.parseInt(idStartEnd[2].trim());

		String pos = inString.substring(posStart,conStart-1);
		String con = inString.substring(conStart);

		LinkedList<String> tmpPositions = convertDotNotation(pos);

		LinkedList<LinkedList<String>> positions = Word.convertWord(tmpPositions);
		LinkedList<String> tmpcontent = convertDotNotation(con);

		content = new LinkedList<Word>();
		Word tmpWord;

		for(int i = 0; i < positions.toArray().length; i++){
			tmpWord = new Word(positions.get(i).get(1),
					positions.get(i).get(2),
					positions.get(i).get(0),
					tmpcontent.get(i));
			content.add(tmpWord);
		}
	};

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

	public static LinkedList<String> convertDotNotation(String inString){
		int l = inString.length();
		int pairs = 0;
		int j=l;
		if (l < 3 || !inString.substring(0,3).equals("'.'")){
			return new LinkedList<String>();
		}
		for (int i = 0; i<l; i++){
			if (inString.substring(i,i+1).equals( "(" ))
					pairs++;
			else if (inString.substring(i,i+1).equals( ")" ))
					pairs--;
			else if (inString.substring(i,i+1).equals( "," )
				&& pairs == 1){
					j = i+2;
					break;
				}
			}
		LinkedList<String> retVal = new LinkedList<String>(convertDotNotation(inString.substring(j,l-1)));
		retVal.addFirst(inString.substring(4,j-2));
		return retVal;
	}

	public static LinkedList<Sentence> convertSentences(String inString){
		LinkedList<String> temp = convertDotNotation(inString);
		LinkedList<Sentence> retVal = new LinkedList<Sentence>();
		Sentence tmp;
		for (String sentence : temp)
			retVal.add(new Sentence(sentence));
		return retVal;
	}
}
