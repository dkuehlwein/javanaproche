
<script type="text/javascript">
// zählt die \ns im string
function lineBreakCount(str){
	/* counts \n */
	try {
		return((str.match(/[^\n]*\n[^\n]*/gi).length));
	} catch(e) {
		return 0;
	}
}

// findSentence: Methode, um einen Bereich im Textfeld zu markieren
// start: Startwert der Markierung
// end: Endwert der Markierung
// Rückgabewert: NA
function findSentence(start, end){
//form id="textgb" method="post" action="/naproche/inc/process_form.php"
//id="idarea" name="txtinput"
	//var lineHeightInPx = 16;
	var lineHeightInPx = 15;
	var input = document.forms['textgb'].elements['txtinput'];
	var e = textgb.txtinput;
	document.getElementById('idarea').scrollTop = lineBreakCount(input.value.substr(0,start)) * lineHeightInPx;
	e.selectionStart = start;
	e.selectionEnd = end;
	e.focus();
}

// sendText: Methode, um einen beliebigen Text in einem bliebigen Objekt einzufügen
// e: Ziel-Objekt
// text: einzufügender Inhalt
// Rückgabewert: NA
function sendText(e, text){

    var selEnd = e.selectionEnd;
    var txtLen = e.value.length;
    var txtbefore = e.value.substring(0,selEnd);
    var txtafter =  e.value.substring(selEnd, txtLen);
    e.value = txtbefore + text + " " + txtafter;

    // Quelle:http://aktuell.de.selfhtml.org/artikel/javascript/bbcode/
    var pos;
    pos = txtbefore.length;
    e.selectionStart = pos + text.length + 1;
    e.selectionEnd = pos + text.length + 1;// so ist das eingefügte Zeichen nicht markiert. 
    e.focus();
}

// insertText: Methode, um einen Bereich im Textfeld zu markieren
// text: inzufügender Inhalt
// Rückgabewert: NA
function insertText(text){
sendText(document.getElementById("idarea"), text);
}


// insertIcon:  Methode, die eine Funktion wie z.B. \frac{}{} oder \begin{..} mit 		\end{...} mit der richtigen Cursurposition einfügt.
//		Klickt man drauf, wird die Methode "instertText" aufgerufen.
// str: Name des Icons & Bezeichnung der Eingabe
// Rückgabewert: NA
function insertMitte(vor,nach)
{
    var e = document.getElementById("idarea");
    var selEnd = e.selectionEnd;
    var txtLen = e.value.length;
    var txtbefore = e.value.substring(0,selEnd);
    var txtafter =  e.value.substring(selEnd, txtLen);
    e.value = txtbefore + vor + "" + nach + txtafter;

    // Quelle:http://aktuell.de.selfhtml.org/artikel/javascript/bbcode/
    var pos;
    pos = txtbefore.length;
    e.selectionStart = pos + vor.length;
// Browserunterschiede: \r\n macht bei Firefox(="Netscape") ein Zeichen zuviel, beim Rest(Opera, IE?) richtig
    if(vor.indexOf("\r") > -1 && navigator.appName == "Netscape")
    {
	e.selectionStart = pos + vor.length-1;
    }
    e.selectionEnd = e.selectionStart;// so ist das eingefügte Zeichen nicht markiert.
    e.focus();
}

function insertMitteMark(vor,nach,wert)
{
    var e = document.getElementById("idarea");
    var selEnd = e.selectionEnd;
    var txtLen = e.value.length;
    var txtbefore = e.value.substring(0,selEnd);
    var txtafter =  e.value.substring(selEnd, txtLen);
    e.value = txtbefore + vor + wert + nach + txtafter;

    var pos;
    pos = txtbefore.length;
    e.selectionStart = pos + vor.length;
    e.selectionEnd = e.selectionStart + wert.length;// so ist das eingefügte Zeichen  markiert.
    e.focus();
}

// insertIcon:  Methode, um Icons darzustellen; 
//		Beim klickt man drauf, wird die Methode "instertText" aufgerufen.
// str: Name des Icons & Bezeichnung der Eingabe
// Rückgabewert: NA
function insertIcon(str)
{
	// Sonderfall: Frac (Bruch) mit zwei Übergabeparametern
	if (str == "frac"){
		document.write("<img src=\"../img/icon/"+str+".GIF\" width=\"25\" height=\"25\" alt=\"\\"+str+"\" onclick=\"insertMitte('\\\\frac{','}{}')\"/>");
	}

	// Sonderfall: sqrt (Wurzel) mit zwei Übergabeparametern
	else if (str == "sqrt"){
		document.write("<img src=\"../img/icon/"+str+".GIF\" width=\"25\" height=\"25\" alt=\"\\"+str+"\" onclick=\"insertMitte('\\\\sqrt{','}')\"/>");
	}
	else if (str == "sqrt2"){
		document.write("<img src=\"../img/icon/"+str+".GIF\" width=\"25\" height=\"25\" alt=\"\\"+str+"\" onclick=\"insertMitte('\\\\sqrt[',']{}')\"/>");
	}

	else if (str == "lemma"){
		document.write("<img src=\"../img/icon/"+str+".GIF\" width=\"100\" height=\"25\" alt=\"\\"+str+"\" onclick=\"insertMitte('\\\\begin{lemma}\\r\\n','\\r\\n\\\\end{lemma}\\r\\n\\\\begin{proof}\\r\\n\\r\\n\\\\end{proof}\\r\\n')\"/>");

	}

	else if (str == "theorem"){
		document.write("<img src=\"../img/icon/"+str+".GIF\" width=\"100\" height=\"25\" alt=\"\\"+str+"\" onclick=\"insertMitte('\\\\begin{theorem}\\r\\n','\\r\\n\\\\end{theorem}\\r\\n\\\\begin{proof}\\r\\n\\r\\n\\\\end{proof}\\r\\n')\"/>");
	}

	else if (str == "axiom"){
		document.write("<img src=\"../img/icon/"+str+".GIF\" width=\"100\" height=\"25\" alt=\"\\"+str+"\" onclick=\"insertMitte('\\\\begin{axiom}\\r\\n','\\r\\n\\\\end{axiom}')\"/>");
	}
	else if (str == "definition"){
		document.write("<img src=\"../img/icon/"+str+".GIF\" width=\"100\" height=\"25\" alt=\"\\"+str+"\" onclick=\"insertMitte('\\\\begin{definition}\\r\\n','\\r\\n\\\\end{definition}')\"/>");	
	}
	else if (str == "N"){
		document.write("<img src=\"../img/icon/"+str+".GIF\" width=\"25\" height=\"25\" alt=\"\\"+str+"\" onclick=\"insertMitteMark('\\\\mathbb{','}','N')\"/>");
	}
	else if (str == "leerbl"){
		document.write("<img src=\"../img/icon/"+str+".GIF\" width=\"25\" height=\"25\" />");
	}
	// Regelfall: Ein Übergabeparameter, mit dem alles eingestellt wird
	else {
		document.write("<img src=\"../img/icon/"+str+".GIF\" width=\"25\" height=\"25\" alt=\"\\"+str+"\" onclick=\"insertText('\\\\"+str+"')\"/>");
	}
}
//document.write("<div id='contentJS'>");
</script> 