<?
/* Einbindung der Datei init.php
Variablen:
$strtmp: String mit Inhalt aus dem Textfeld
$prolog: Textinhalt auf Prolog angepasst
*/  
//require_once('init.php'); //zum aufruf von php
//include('init.php'); //zum aufruf von php
//include($_SERVER["DOCUMENT_ROOT"].'/naproche/tmp/'.$sesid.'/error.php'); //zum aufruf von php
//echo "Error: ".$prologErrorList;
$test = 0;
$MenuMark = "rgb(238,238,238)"; // 238,238,238 <=> #EEEEEE
$MenuUnMark = "rgb(70,123,167)";//70,123,167 <=> #467ba7
$MenuMarkHex = '"#EEEEEE"';
$MenuUnMarkHex = '"#467ba7"';
// debugmode alles an
// farben anpassen
?>

<script type="text/javascript">

// Die Klasse "sentences"
// id: SatzID
// content: Inhalt des Satzes
// color: Farbe, in der der Inhalt des Satzes angezeigt werden soll
function sentences(nr,con,c){
	this.id = nr;
	this.content =con;
	this.details = "";
	this.color=c;
}

// Schreibt die Details des Satzes mit ID in HTML-Stil auf eine Website
sentences.prototype.write=function(){
	document.writeln("<div id='"+this.id+"' onClick='this.v=show(\""+this.id+"\")';>");
	// Ausgabe Inhalt
	document.writeln(this.content);
	document.writeln("</div>");
	// Vorbereitung Details
	document.writeln("<div id='details"+this.id+"'>");
	document.writeln("</div>");
}

// Setzt das Attribut details der Sentences
// d: die zu setzenden Details
sentences.prototype.setDetails=function(d){
	try{
		// Vorbereitung Tags
		var detID = "details"+this.id;
		var kopf = "<div id = " + detID + ">";
		var fuss = "</div>";
		var dbm = "<?echo $debugMode;?>";

		this.details = this.details + kopf + d + fuss;
		// HTML in ein Tag einbauen
		document.getElementById(detID).innerHTML = this.details;

		var id = this.id;
		if (dbm == "on"){
			document.getElementById("details" + id).style.display = "block";
		}
		else if(document.getElementById("details" + id).style.display != "none"){
			document.getElementById("details" + id).style.display = "none";
		}
	}
	catch(e){
		alert("Error:\n " + e + "\n");
	}
}

// setcolor(result)
// Changes the color of a sentence in the output according to Result.
// If the sentence is already colored, the new color is set according to the following color priority:
// red > orange > green > black
sentences.prototype.setColor=function(result){
	if ( result == "noproof" || this.color == "#FF2222") {
		this.color="#FF2222"; 
	}
	else if ( result == "warning" || this.color == "#FF8800" ) {
		this.color="#FF8800";
	}
	else if ( result == "theorem" || this.color == "#006600" ) {
		this.color = "#006600";
	}
	else {
		this.color = "#000000";
	}

	document.getElementById(this.id).setAttribute('style', 'Color:'+this.color+';');
	//document.getElementById(this.id).style.color = "'#"+this.color+"'";
	document.getElementById(this.id).style.cursor = 'pointer';
	//document.getElementById(this.id).setAttribute('style', 'cursor: pointer');
}

// Ändert die Anzeige der Details zu einem ausgewählten Satz
// id: ID des ausgewählten Satzes
// Rückgabewert: NA
function show(id){
	try{
		if(document.getElementById("details" + id).style.display != "none"){
			document.getElementById("details" + id).style.display = "none";
		}
		else {
			document.getElementById("details" + id).style.display = "block";
		}
	}
	catch(e){
		alert("Error: " + e);
	}
}

// zählt die \ns im string
// str: zu untersuchender String
// Rückgabewert: NA
function lineBreakCount(str){
	try {
		//erklärung: /gi: globale suche im string, die (wegen i) groß un kleinschreibung ignoriert
		// regläre Ausdrücke: http://de.selfhtml.org/javascript/objekte/regexp.htm
		return((str.match(/[^\n]*\n[^\n]*/gi).length));
		// test: s: weissraumzeichen
		//return((str.match(/[^\s]*\s[^\s]*/gi).length));

	} catch(e) {
		return 0;
	}
}
/* Quelle: 
http://blog.sarathonline.com/2009/09/javascript-maxlength-for-textarea-with.html
//*/
function rn(str) {
 if (!str)
  return -1;

 var lns = str.match(/[^\r]\n/g);

 if (lns)
  return lns.length;
 else

  return 0;
}


//Zählt Anzahl der Zeichen sign im String str
function countSign(str,sign){
	try {
		var anz = 0;
		var tmp = str;
		var pos = tmp.indexOf(sign);
		//alert("pos: " + pos + "\nTMP: " + tmp);
		while(pos > 0) {
			tmp = tmp.substring(pos,tmp.length);
			pos = tmp.indexOf(sign)+1;
			anz++;
			//alert("pos: " + pos + "\nTMP: " + tmp);
		}
		//alert("Str: " + str + "\nTMP: " + tmp + "\nAnz: " + anz);
		return(anz);
	} catch(e) {
		return 0;
	}
}

// Markiert einen Satz anhand der SatzID
// t: SatzID
// Achtung: Unterschied zwischen den Browsern
// Rückgabewert: NA
function markSentence(t){
	var satzPos=new Array();
	
	//document.writeln(t);
	// Übertrage die Werte des PHP-Arrays in ein JS-Array
	<?
	for ($i = 0; $i < sizeof($pkt); $i++){
		echo "satzPos[".$i."] = ".$pkt[$i]."; ";
		//echo "document.writeln(satzPos[".$i."]);";
	}
	?>
	// Sonderfall Netscape/Firefox...
	if(navigator.appName == "Netscape"){

		// Inhalt des Textfeldes in die JS-Variable "alles" umwandeln
		// ! entspricht einem Zeilenumbruch
		<? 
			$x = str_replace("!",".",$strtmp);
			//$x = str_replace("\r","##r##",$x);
			//$x = str_replace("\n","##n##",$x);
			$x = str_replace("\n","!",$x);
			$x = str_replace("\r","?",$x);
			//$x = str_replace("\r","#",$x);
			//$x = str_replace("\n","#",$x);
			echo "var alles=\"".$x."\";";
		?>
		// Werden \ns zuwenig gezählt?
		var tmp = alles.substring(0,satzPos[t-1]);
		//alert(tmp);
		//var anz = countSign(tmp,"##r####n##");
		var anz = countSign(tmp,"!");
		tmp = "";
		tmp = alles.substring(satzPos[t-1],satzPos[t]);
		var drin = countSign(tmp,"!");
		//alert("Anzahl vor:" + anz + "\ndrinnen: " +drin);
		var start = satzPos[t-1]-anz;
		var ende = satzPos[t]-anz;
		//var anz = countSign(tmp,"##");
		if (drin > 0){
			//alert(drin + " PUNKT("+start+"):" + alles.charAt(start+4) + alles.charAt(start+3) + alles.charAt(start+2) + alles.charAt(start+1) + "|" + alles.charAt(start-0) + alles.charAt(start-1) + alles.charAt(start-2) + alles.charAt(start-3) + alles.charAt(start-4) + alles.charAt(start-5) + alles.charAt(start-6) + 	alles.charAt(start-7));
		}
		
/*
		if(alles.charAt(satzPos[t-1]-anz-1) == '.'){
			alert("PUNKT!");
			start = satzPos[t-1]-anz+drin;
			ende = satzPos[t]-anz+drin;
		}
*/
		markArea(start,ende);
		//alert("Markierungskoordinaten: " + start + "/"+ ende);
	}
	// Normalfall; z.B. Opera
	else {
		//findSentence(satzPos[t-1],satzPos[t], t);
		markArea(satzPos[t-1],satzPos[t]);
	}
}



function markArea(start,end){
	var wi = document.getElementById("idarea").clientWidth;
	var lineHeightInPx = 16 + 3; // 16: schriftgröße; 3: zeilentoleranz 
	var signWidthInPx = 10; // Ein Zeichen in der Textarea ist (ca) 10 Pixel
	var signsWidth = parseInt(wi / signWidthInPx); // ganzzahliger Wert
	var textInhalt = document.getElementById("idarea").value.replace(/(\015\012)|(\015)|(\012)/g, '\n');
	// Zerlegt Zeichenketten in ein Array
	var textArray = textInhalt.split('\n'); 
	var backslashn = 0;
	// grenzwert für die do-while Schleife
	var Z = lineBreakCount(textInhalt.substr(0,start));
	var ZZ = rn(textInhalt.substr(0,start));
	//alert("Umbrüche: " + Z + "|" + ZZ);
	var umbruch = 0;

	// nicht bis zum Ende, sondern bis zur Satzposition
	while (backslashn < Z){
		backslashn = backslashn + 1;
		var texttmp = textArray[backslashn];
		umbruch = umbruch + parseInt(texttmp.length / signsWidth);
	}

	var n = (backslashn + umbruch) - (20 / 2);// 20 Zeilen Höhe 

	var e = document.getElementById("idarea");
	//alert("Markierungskoordinaten: " + start + "/"+ end);
	e.selectionStart = start;
	e.selectionEnd = end;
	e.focus();
	document.getElementById('idarea').scrollTop = n * lineHeightInPx;
	document.getElementById('content').scrollTop = 0;
}

// findSentence: Methode, um einen Bereich im Textfeld zu markieren
// start: Startwert der Markierung
// end: Endwert der Markierung
// Rückgabewert: NA
function findSentence(start, end, id){
	var wi = document.getElementById("idarea").clientWidth;
	var lineHeightInPx = 16 + 3; // 16: schriftgröße; 3: zeilentoleranz 
	var signWidthInPx = 10; // Ein Zeichen in der Textarea ist (ca) 10 Pixel breit!
	//document.getElementById(idarea).value.replace(/(\015\012)|(\015)|(\012)/g, '\n'); 
	
	var rows = 0;
	var cols = 0;
	var signsWidth = parseInt(wi / signWidthInPx); // ganzzahliger Wert

	var textInhalt = document.getElementById("idarea").value.replace(/(\015\012)|(\015)|(\012)/g, '\n');
	// Zerlegt Zeichenketten in ein Array
	var textArray = textInhalt.split('\n'); 
	var backslashn = 0;
	// grenzwert für die do-while Schleife
	var Z = lineBreakCount(textInhalt.substr(0,start));
	//alert("Umbrüche: " + Z);
	var umbruch = 0;

	// nicht bis zum Ende, sondern bis zur Satzposition
	while (backslashn < Z){
		backslashn = backslashn + 1;
		var texttmp = textArray[backslashn];
		umbruch = umbruch + parseInt(texttmp.length / signsWidth);
	}

	var n = (backslashn + umbruch) - (20 / 2);// 20 Zeilen Höhe 
	//var input = document.forms['textgb'].elements['txtinput'];
	var e = document.getElementById("idarea");

	var delta = 0;
/*
	if(navigator.appName == "Netscape"){
		textInhalt = document.getElementById("idarea").value;
		alert(textInhalt.substr(start,end));
		//delta = lineBreakCount(textInhalt.substr(start,end));
	}
//*/
	e.selectionStart = start;
	e.selectionEnd = end;
	e.focus();
	
	// Markieren und entsprechend Scrollen
	document.getElementById('idarea').scrollTop = n * lineHeightInPx;
}




// insertText: Methode, um einen Bereich im Textfeld zu markieren
// text: inzufügender Inhalt
// Rückgabewert: NA
function insertText(text){
	//sendText(document.getElementById("idarea"), text);
	var e = document.getElementById("idarea");
	var selEnd = e.selectionEnd;
	var txtLen = e.value.length;
	var txtbefore = e.value.substring(0,selEnd);
	var txtafter =  e.value.substring(selEnd, txtLen);
	e.value = txtbefore + text + " " + txtafter;
	
	var pos;
	pos = txtbefore.length;
	e.selectionStart = pos + text.length + 1;
	e.selectionEnd = pos + text.length + 1;// so ist das eingefügte Zeichen nicht markiert. 
	e.focus();
}

// insertIcon:  Methode, die eine Funktion wie z.B. \frac{}{} oder \begin{..} mit 		\end{...} mit der richtigen Cursurposition einfügt.
//		Klickt man drauf, wird die Methode "instertText" aufgerufen.
// str: Name des Icons & Bezeichnung der Eingabe
// Rückgabewert: NA
function insertMitte(vor,nach){
	var e = document.getElementById("idarea");
	var selEnd = e.selectionEnd;
	var txtLen = e.value.length;
	var txtbefore = e.value.substring(0,selEnd);
	var txtafter =  e.value.substring(selEnd, txtLen);
	e.value = txtbefore + vor + "" + nach + txtafter;
	
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

function insertMitteMark(vor,nach,wert){
	var e = document.getElementById("idarea");
	var selEnd = e.selectionEnd;
	var txtLen = e.value.length;
	var txtbefore = e.value.substring(0,selEnd);
	var txtafter =  e.value.substring(selEnd, txtLen);
	e.value = txtbefore + vor + wert + nach + txtafter;
	
	var pos;
	pos = txtbefore.length;
	e.selectionStart = pos + vor.length;
	e.selectionEnd = e.selectionStart + wert.length;// so ist das eingefügte Zeichen markiert.
	e.focus();
}

function insertTransparent(str){
	if(str == "N"){
		document.write("<img class='unmark' src=\"../img/icon/"+str+".png\" onclick=\"insertMitteMark('\\\\mathbb{','}','N')\" onMouseOver=\"this.className='mark';\" onMouseOut=\"this.className='unmark';\"/>");
	}
	else if(str == "sqrt"){
		document.write("<img class='unmark' src=\"../img/icon/"+str+".png\" onclick=\"insertMitte('\\\\sqrt{','}')\" onMouseOver=\"this.className='mark';\" onMouseOut=\"this.className='unmark';\"/>");
	}
	else if(str == "sqrt2"){
		document.write("<img class='unmark' src=\"../img/icon/"+str+".png\" onclick=\"insertMitte('\\\\sqrt[',']{}')\" onMouseOver=\"this.className='mark';\" onMouseOut=\"this.className='unmark';\"/>");
	}
	else if(str == "frac"){
		document.write("<img class='unmark' src=\"../img/icon/"+str+".png\" onclick=\"insertMitte('\\\\frac{','}{}')\" onMouseOver=\"this.className='mark';\" onMouseOut=\"this.className='unmark';\"/>");
	}
	else if(str == "axiom"){
		document.write("<img class='unmark' src=\"../img/icon/"+str+".png\" onclick=\"insertMitte('\\\\begin{axiom}\\r\\n','\\r\\n\\\\end{axiom}')\" onMouseOver=\"this.className='mark';\" onMouseOut=\"this.className='unmark';\"/>");
	}
	else if(str == "definition"){
		document.write("<img class='unmark' src=\"../img/icon/"+str+".png\" onclick=\"insertMitte('\\\\begin{definition}\\r\\n','\\r\\n\\\\end{definition}')\" onMouseOver=\"this.className='mark';\" onMouseOut=\"this.className='unmark';\"/>");
	}
	else if(str == "lemma"){
		document.write("<img class='unmark' src=\"../img/icon/"+str+".png\" onclick=\"insertMitte('\\\\begin{lemma}\\r\\n','\\r\\n\\\\end{lemma}\\r\\n\\\\begin{proof}\\r\\n\\r\\n\\\\end{proof}\\r\\n')\" onMouseOver=\"this.className='mark';\" onMouseOut=\"this.className='unmark';\"/>");
	}
	else if(str == "theorem"){
		document.write("<img class='unmark' src=\"../img/icon/"+str+".png\" onclick=\"insertMitte('\\\\begin{theorem}\\r\\n','\\r\\n\\\\end{theorem}\\r\\n\\\\begin{proof}\\r\\n\\r\\n\\\\end{proof}\\r\\n')\" onMouseOver=\"this.className='mark';\" onMouseOut=\"this.className='unmark';\"/>");
	}
	else {
		document.write("<img class='unmark' src=\"../img/icon/"+str+".png\" onclick=\"insertText('\\\\" +str+ "')\" onMouseOver=\"this.className='mark';\" onMouseOut=\"this.className='unmark';\"/>");
	}
}

// Öffnet das Symbol-Menu 
// tabIndex: zu öffnende Menu-Position
// Rückgabewert: NA
function openRibbonMenu(tabIndex){
	var ribbonTabsMax = 4;
	for(var ribbonCount = 1; ribbonCount <= ribbonTabsMax; ribbonCount++){
		if(ribbonCount == tabIndex){
			if (document.getElementById("ribbonContentTab" + ribbonCount).style.display == "block"){
				document.getElementById("ribbonMenuTab" + ribbonCount).childNodes[0].style.color = <?echo $MenuMarkHex;?>;
				document.getElementById("ribbonMenuTab" + ribbonCount).childNodes[0].style.backgroundColor = <?echo $MenuUnMarkHex;?>;
				document.getElementById("ribbonContentTab" + ribbonCount).style.display = "none";
			}
			else{
				document.getElementById("ribbonMenuTab" + ribbonCount).childNodes[0].style.color = <?echo $MenuUnMarkHex;?>;
				document.getElementById("ribbonMenuTab" + ribbonCount).childNodes[0].style.backgroundColor = <?echo $MenuMarkHex;?>;
				document.getElementById("ribbonContentTab" + ribbonCount).style.display = "block";
			}
		}
		else{
			document.getElementById("ribbonMenuTab" + ribbonCount).childNodes[0].style.color = <?echo $MenuMarkHex;?>;
			document.getElementById("ribbonMenuTab" + ribbonCount).childNodes[0].style.backgroundColor = <?echo $MenuUnMarkHex;?>;
			document.getElementById("ribbonContentTab" + ribbonCount).style.display = "none";
		}
	}
}
</script> 

<div id="content">
	<div id = "warerr">
<?PHP // evtl. Fehlerausgabe
	if (strlen($warning) > 0) {
		//echo "<br/>TMP:<br/>".$tmptxt."<br/>";
		echo "<br/>Warnings:<br/>".$warning."<br/>\r\n";

	}
	if (strlen($error) > 0) {
		echo "<br/>Error:<br/>".$error."<br/>\r\n";
	}
	if (strlen($prologErrorList) > 0) {
		echo "<br/>An Error occurred: Details listed below<br/><br/>\r\n";
	}
?>
	</div>

<form id="textgb" method="post" action="../inc/process_form.php"> 
	<div id="desc">
		<?//echo "<h2>Insert your text here:</h2>"?>

<script type="text/javascript">

document.write("<div id=\"navpic\">");
//document.write("<br/>");
</script>
<div id="pici">
<ul class="ribbonMenu">

 <li id="ribbonMenuTab1"><a style="color: <?echo $MenuMark?>; background-color: <?echo $MenuUnMark?>;" href="javascript:openRibbonMenu(1);">Enviroments</a></li>

 <li id="ribbonMenuTab2"><a style="color: <?echo $MenuMark?>; background-color: <?echo $MenuUnMark?>;" href="javascript:openRibbonMenu(2);">Arrows</a></li>

 <li id="ribbonMenuTab3"><a style="color: <?echo $MenuMark?>; background-color: <?echo $MenuUnMark?>;" href="javascript:openRibbonMenu(3);">Relations</a></li>

 <li id="ribbonMenuTab4"><a style="color: <?echo $MenuMark?>; background-color: <?echo $MenuUnMark?>;" href="javascript:openRibbonMenu(4);">Symbols</a></li>

<?// <li id="ribbonMenuTab5"><a style="color: rgb(0, 0, 0); background-color: rgb(255, 255, 255);" href="javascript:openRibbonMenu(5);">Rest</a></li>?>
</ul>

</div>
<br/>
<ul class="ribbonContent">

 <li style="display: none;" id="ribbonContentTab1">
<table><tr>
<td><script type="text/javascript">javascript:insertTransparent("lemma");</script></td>
<td><script type="text/javascript">javascript:insertTransparent("axiom");</script></td>
<td><script type="text/javascript">javascript:insertTransparent("theorem");</script></td>
<td><script type="text/javascript">javascript:insertTransparent("definition");</script></td>
</tr></table>
  </li>

 <li style="display: none;" id="ribbonContentTab2">
<table><tr>
<td><script type="text/javascript">javascript:insertTransparent("implies");</script></td>
<td><script type="text/javascript">javascript:insertTransparent("leftrightarrow");</script></td>
<td><script type="text/javascript">javascript:insertTransparent("rightarrow");</script></td>
</tr></table>
 </li>

 <li style="display: none;" id="ribbonContentTab3">
<table><tr>
<td><script type="text/javascript">javascript:insertTransparent("geq");</script></td>
<td><script type="text/javascript">javascript:insertTransparent("equi");</script></td>
<td><script type="text/javascript">javascript:insertTransparent("vee");</script></td>
<td><script type="text/javascript">javascript:insertTransparent("cup");</script></td>
<td><script type="text/javascript">javascript:insertTransparent("supset");</script></td>
<td><script type="text/javascript">javascript:insertTransparent("leq");</script></td>
<td><script type="text/javascript">javascript:insertTransparent("neq");</script></td>
<td><script type="text/javascript">javascript:insertTransparent("wedge");</script></td>
<td><script type="text/javascript">javascript:insertTransparent("cap");</script></td>
<td><script type="text/javascript">javascript:insertTransparent("subset");</script></td>
</tr></table>
 </li>

 <li style="display: none;" id="ribbonContentTab4">
<table><tr>
<td><script type="text/javascript">javascript:insertTransparent("exists");</script></td>
<td><script type="text/javascript">javascript:insertTransparent("in");</script></td>
<td><script type="text/javascript">javascript:insertTransparent("N");</script></td>
<td><script type="text/javascript">javascript:insertTransparent("sqrt");</script></td>
<td><script type="text/javascript">javascript:insertTransparent("frac");</script></td>
<td><script type="text/javascript">javascript:insertTransparent("emptyset");</script></td>
<td><script type="text/javascript">javascript:insertTransparent("forall");</script></td>
<td><script type="text/javascript">javascript:insertTransparent("neg");</script></td>
<td><script type="text/javascript">javascript:insertTransparent("cdot");</script></td>
<td><script type="text/javascript">javascript:insertTransparent("sqrt2");</script></td>
</tr></table>
 </li>

</ul>
<script type="text/javascript">
document.write("</div>");

</script>
	</div>	
	<div id="main">

	<div class="input">
	<textarea id="idarea" name="txtinput" rows="10" cols="60"><?PHP echo $strtmp;?></textarea>
	</div>

	<div id="function">
		<input type="submit" name="button" value="create PDF"/> 
		<?
			if($debugMode == "on"){
				echo '<input type="submit" name="button" value="Preparse Input"/>';
				echo '<input type="submit" name="button" target="prs" value="Create PRS"/> ';
			}
		?>
		<input type="submit" name="button" value="Logical Check"/> 
		<?
			if($debugMode == "on"){
				echo '<input class="DBMon" type="submit" name="button" value="Debug-Mode '.$debugMode.'"/>';
			}
			else if($debugMode == "off"){
				echo '<input class="DBMoff" type="submit" name="button" value="Debug-Mode '.$debugMode.'"/>';
			}
		?>
		<input type="hidden" name="id" value="<?=session_id()?>"/>
		<input type="hidden" name="DBM" id="DBM" value="<?echo $debugMode?>"/>
	</div>
	<?
		if($debugMode == "on"){
	?>
	<div id="settings">
		<br/>
		<table border="0">
			<colgroup>
				<col width="10%"/>
				<col width="30%"/>
				<col width="30%"/>
				<col width="20%"/>
			</colgroup>
			<tr>
				<td>
					Additional <br/>Settings: 
				</td>
				<td>
					<div id="prover">
						Select Prover:
						<select name="sprover">
<option value="EP---" <? if($sprover=="EP---") echo "selected='selected'"; ?>>EP---</option>
<option value="Metis---" <? if($sprover=="Metis---") echo "selected='selected'"; ?>>Metis---</option>
<option value="Vampire---" <? if($sprover=="Vampire---") echo "selected='selected'"; ?>>Vampire---</option>
<option value="Otter---3.3" <? if($sprover=="Otter---3.3") echo "selected='selected'"; ?>>Otter---3.3</option>
						</select>
					</div>
				</td>
				<td>
					<div id="location">
						Select Prover Location:
						<select name="plocation">
<option value="local" <? if($plocation=="local") echo "selected='selected'"; ?>>local</option>
<option value="external" <? if($plocation=="external") echo "selected='selected'"; ?>>external</option>
						</select>
					</div>
				</td>
				<td>
					
				</td>

			</tr>
			<tr>
				<td></td>
				<td>
					<div id="time">
						Max Time per Obligation:
						<input type="text" size="2" name="tpo" align="right" value="<? echo $tpo;?>"></input> seconds. 
					</div>
				</td>
				<td>
					<div id="outputLvl">
						Maximal Premise Distance:
						<input type="text" size="5" name="soutput" align="right" value="<? echo $soutput;?>"></input>
					</div>
				</td>
			</tr>
		</table>
	</div>
	<?
		}
		else{
			$sprover = $sprover;
			$plocation = $plocation;
			$tpo = $tpo;
			$soutput = $soutput;
		}
	?>
</div>
</form>