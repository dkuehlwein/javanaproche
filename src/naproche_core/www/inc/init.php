<?PHP 
error_reporting(0);
ini_set('display_errors', 1);
//require_once('datei.php'); //zum aufruf von php

// Initialisierung der PHP-Werte:
session_start();
$sesid = session_id();
// Tmp-Verzeichniss mit Session-ID
mkdir('../tmp/'.$sesid);

$strtmp ="% Try this example or insert your own text.

Axiom.
There is no \$y$ such that \$y \in \emptyset$.

Define \$x$ to be transitive if and only if for all \$u$, \$v$, if \$u \in v$ and \$v \in x$ then \$u\in x$.
Define \$x$ to be an ordinal if and only if \$x$ is transitive and for all \$y$, if \$y \in x$ then \$y$ is transitive.

Then $\emptyset$ is an ordinal.";


//echo "DBM: ".$debugMode."<br/>";
$debugMode = $_GET['DBM'];	

// Variablen des Formlares:
$button = $_POST["button"];
if ($button == "Debug-Mode on"){
	$debugMode = "on";
}
else if ($button == "Debug-Mode off"){
	$debugMode = "off";
}
else if ($_POST["DBM"] == "on"){
	$debugMode = "on";
}
else if ($_POST["DBM"] == "off"){
	$debugMode = "off";
}

// wenn $tpo nicht gesetzt wurde, ists 5.
$tpo = $_POST["tpo"];
if (strlen($tpo)<1){
	$tpo = "5";
}

// wenn $soutput nicht gesetzt wurde, ists 20.
$soutput = $_POST["soutput"];
if (strlen($soutput)<1){
	$soutput = "20";
}

// wenn sprover nicht gesetzt wurde, ists EP--.
$sprover = $_POST["sprover"];
if (strlen($sprover)<1){
	$sprover = "EP---";
}

$plocation = $_POST["plocation"];
if (strlen($plocation)<1){
	$plocation = "local";
}

// wenn Inhalt im Textfeld vorhanden ist
$inh ="";
if (isset($_POST["txtinput"])){
	// verhindere doppelte '\'
	$strtmp = stripslashes($_POST["txtinput"]);
	
	// Überprüfung von Eingabefehlern
	include('check.php'); //zum aufruf von php
	if (strlen($error)>0){
		//echo "Fehler aufgetreten!<br/>";
	}
	// Normalfall
	else {
	
		//$strtmp = str_replace("\r\n\r\n\r\n","\r\n\r\n",$strtmp);
		$prolog = $strtmp;
	
		// löschen aller mehrzeiligen Kommentare und speichern der gelöschten positionen 
		$posi = -1;
		$j = 1;
		$mehrkomm[0][0] = -1;
		$mehrkomm[0][1] = -1;
	
		// Latex-style umwandeln:
		// Hier abfangen der Texmacs-Bereiche
		// ersetzen der latexelemente \begin{} für: 
		//\begin{}:		##beg##.	// punkt  am ende hinter "objekt"
		//\end{}:		##e##.		// punkt  am ende hinter "objekt"
		// - axiom: 		##a##
		// - theorem: 		##the##
		// - proof: 		##p##
		// - lemma: 		##l##
		// - definition:	##defini##
	//Soll: "\begin[41a]{axiom}" <=> "sentence(#,[axiom, '41a'])."
		$prolog = str_replace("\\begin","#beg##",$prolog);
		$prolog = str_replace("{axiom}","###a##.",$prolog);
		$prolog = str_replace("{theorem}","###the##.",$prolog);
		$prolog = str_replace("{proof}","###p##.",$prolog);
		$prolog = str_replace("{lemma}","###l##.",$prolog);
		$prolog = str_replace("{definition}","###defini##.",$prolog);

		$prolog = str_replace("\\end","#e##",$prolog);
		$prolog = str_replace("{axiom}","###a##.",$prolog);
		$prolog = str_replace("{theorem}","###the##.",$prolog);
		$prolog = str_replace("{proof}","###p##.",$prolog);
		$prolog = str_replace("{lemma}","###l##.",$prolog);
		$prolog = str_replace("{definition}","###defini##.",$prolog);
	
		do{
			$posi = strpos($prolog,"/*");
			$mehrkomm[$j][0] = $posi;
			if ($posi > -1){
				$posii = strpos($prolog,"*/",$posi) + 2; // */ 2 zeichen
				$mehrkomm[$j][1] = $posii;
				$mitte = substr($prolog, $posi, $posii);
				$mitte = substr($mitte,0,$posii-$posi);
				//echo "Section:<br>".$mitte."<br>start: ".$posi."<br>ende: ".$posii."<br> länge: " .strlen($mitte)."<br>";
				//echo "Alles:<br>".$prolog."<br>";
				$prolog = str_replace($mitte,"",$prolog);
			}
			$j = $j + 1;
		}while($posi > -1);
		//echo "Kommentarpos: ".$mehrkomm[$j-1][0]." , ".$mehrkomm[$j-1][1]."<br/>";
	
		// löschen aller einzeiligen Kommentare und speichern der gelöschten Positionen
		$j = 1;
		$ezkomm[0][0] = -1;
		$ezkomm[0][1] = -1;	
		$posi = -1;
		do{
			$posi = strpos($prolog,"%");
			$ezkomm[$j][0] = $posi;
			if ($posi > -1){
				$posii = strpos($prolog,"\r\n",$posi);
				$ezkomm[$j][1] = $posii;
				$mitte = substr($prolog, $posi, $posii-$posi);
				//echo "Section:<br/>".$mitte."<br/>start: ".$posi."<br/>ende: ".$posii."<br/> länge: " .strlen($mitte)."<br>";
				$prolog = str_replace($mitte,"",$prolog);
				//echo "Alles:<br/>".$prolog."<br/><br/><br/>";
			}
			$j = $j + 1;
		}while($posi > -1);
		//$parseContent = $prolog;
		$prolog = str_replace("i.e.","#ie#",$prolog);
		// theoretisch ab hier alle relevanten Sätze:
	
		//ermittle Satzstruktur
		// $pkt-Array enthält String-Positionen aller Punkte
		$pkt[0] = 0;
		$x = $prolog;
		$x = str_replace(":",".",$x);
		$pktpos = 0;
		$tmplgth = 0;
		$j = 1;
		$jk = 1;
		do {
			$pktpos = strpos($x,".");
			//$tmplgth = $tmplgth + $pktpos + 1;
			$tmplgth = $tmplgth + $pktpos+1;
			$pkt[$j-1] = $pkt[$j-1];
			$pkt[$j] = $tmplgth;
			$x = substr($x,$pktpos+1,strlen($x));
			$j = $j + 1;
		} while ($pktpos > -1);
	
		// vergleiche alle punkte mit den mehrzeiligen kommentaren
		for ($j = 1; $j < sizeof($mehrkomm); $j++){
			$diff = $mehrkomm[$j][1] - $mehrkomm[$j][0];
			if ($diff > 0){
				//echo "j: ".$j.":".$mehrkomm[$j][0].":".$mehrkomm[$j][1].":".$diff."<br/>";
				for($jj = 1; $jj < sizeof($pkt); $jj++){
					if ($pkt[$jj] > $mehrkomm[$j][0]){
						$pkt[$jj] = $pkt[$jj] + $diff;
					}
				}
			}
		}

		// vergleiche alle punkte mit den einzeiligen kommentaren TBC-Fehler!
		for ($j = 1; $j < sizeof($ezkomm); $j++){
			$diff = $ezkomm[$j][1] - $ezkomm[$j][0];
			
			if ($diff > 0){
				for($jj = 1; $jj < sizeof($pkt); $jj++){
					if ($pkt[$jj] > $ezkomm[$j][0]){
						$pkt[$jj] = $pkt[$jj] + $diff;// wegen \r\n
						//echo "Diff:".$diff."<br/>";
						//echo "PKT-size:".sizeof($pkt)."<br/>";
					}
				}
			}
		}

		// Schleife zum korrekten zuordnen eventueller Aufzählungen
		$prolog = str_replace("\r\n"," ",$prolog);
		$prolog = str_replace("}","} ",$prolog);
		$prolog = str_replace("} {","}{",$prolog);
		do{
			$posend = 0;
			$posanf = strpos($prolog,"##[",$posend);
			if ($posanf > -1){
				$posend = strpos($prolog,"]##",$posanf);
				
				if ($posend > -1){
					$mitte = substr($prolog,$posanf + 3, $posend-$posanf-3);
					//echo "mitte: ".$mitte."<br/>\r\n";
					$prolog = str_replace("[".$mitte."]","",$prolog);
					$split = strpos($prolog,"##.",$posend-strlen($mitte));
					$vorher = substr($prolog,0,$split+2);// bis vor dem letzten Punkt
					$nachher = substr($prolog,$split+2,strlen($prolog));
					//echo "Satz mit Bemerkung: ".$vorher.$mitte.$nachher."<br/>\r\n";
					$prolog = $vorher.$mitte.$nachher;
				}
			}
		}while($posanf>-1);

		// Vorbereiten der Großbuchstaben in Kleinbuchstabenumwandlung im nicht-Mathe-Bereich
		$prolog = str_replace('  ', ' ', preg_replace("/[A-Z]/", "#".'\0'."#", $prolog));
		
		// entferne alle doppelten Leerzeichen des Textinhalts im Prolog-Code
		$posleer = strpos($prolog,"  ");
		while($posleer > -1){
			$prolog = str_replace("  "," ",$prolog);
			$posleer = strpos($prolog,"  ");
		}
		$prolog = str_replace("  "," ",$prolog);

		// ersetze sentences
		// Aufteilung anhand von Satzzeichen . und : 
		// Ausnahme i.e.
		//$prolog = $text;
		$prolog = str_replace("#ie#","##ie##",$prolog);
		$prolog = str_replace(",","##komma## ",$prolog);
		$prolog = str_replace("'","##succ##",$prolog);
		$prolog = str_replace("`","##succ##",$prolog);
		$prolog = str_replace(":",".",$prolog);
		//$prolog = str_replace('  ', ' ', preg_replace("/[A-Z]/", "#".'\0'."#", $prolog));
		$prolog = str_replace("\\\\","",$prolog);
		
		// TBC: ersetzen aller punkte im Mathebereich.
		$matheBereich = 0;
		for($i = 0; $i < strlen($prolog); $i++){
			if($prolog[$i] == "$"){
				if($matheBereich == 0) $matheBereich = 1;
				else if($matheBereich == 1) $matheBereich = 0;
			}
			if($matheBereich == 1){
				if($prolog[$i] == "."){
					$prolog[$i] = "?";
				}
			}
		}
	
	// Trenne nach Punkten
	if((strpos($prolog,".")>-1)){
		$anzp = strlen($prolog) - strlen(str_replace(".", "", $prolog));
		if ($anzp == 1) {
			$pos = strpos($prolog,".");
			$vorher = "sentence(1,['".substr($prolog,0,$pos)."']).";
			$rest = trim(substr($prolog,$pos+1,strlen($prolog)));
			if (strlen($rest) > 0) {
				//$warning = "Missing full stop: '".$rest."' not allocated!";
				//echo "Warning: ".$warning."<br>\r\n";
			}
			$prolog = $vorher;
			//echo "inhalt: ".$prolog."<br>\r\n";
			//echo "rest: ".$rest."<br>\r\n";
		}
		else{
			$prolog = str_replace(".","##trennen##",$prolog);
	
			// Erzeugung der sentence-struktur anhand der Trennsymbole
			$count = 2;
			$trenn = "##trennen##";
			//$trenn = ".";
			do{
				$posi = strpos($prolog,$trenn);
				if ($posi > -1){
					$vorher = trim(substr($prolog,0,$posi));
					$mitte = trim(substr($prolog,$posi+strlen($trenn)));
					$nachher = "";
					if (strpos($mitte,$trenn)>-1){
						$nachher = trim(substr($mitte, strpos($mitte,$trenn)+strlen($trenn), strlen($mitte)));
						if (strlen($nachher) > 0) {
							$nachher = $trenn.$nachher;
						}
						$mitte = substr($mitte, 0, strpos($mitte,$trenn));
					}	
					if(strlen($mitte)>0){
						$prolog = $vorher."sentence(".$count.",['".$mitte."']).".$nachher;
						$count++;
					}
					else{
					$prolog = $vorher.$nachher;
					}
				}
				else {
					$mitte = trim(substr($prolog,0,strpos($prolog,"sentence(2,")));
					$nachher = trim(substr($prolog, strlen($mitte), strlen($prolog)));
					if(strlen($mitte)>1){
						$mitte = "sentence(1,['".$mitte."']).";
						
						$prolog = $mitte.$nachher;		
					}
				}
			
			} while ($posi > -1); 
		}
		// ersetzen der konstanten Ausdrücke
		$prolog = str_replace("##ie##","'i.e.'",$prolog);
		$prolog = str_replace(").",").\r\n",$prolog);
		//$prolog = substr($prolog,0,strlen($prolog)-3); // letztes , weg
		//$prolog = $prolog."]"; // schliessende ] 
		//$prolog = strtolower($prolog); // alles kleinbuchstaben
		$prolog = str_replace(" ","','",$prolog);
		//$prolog = str_replace(", ,",", ",$prolog);
		//$prolog = str_replace("##komma##",", ',' ",$prolog);
	
	}

	// markiere Mathemodus
		//echo "Prolog-code:<br/>\r\n ".$prolog."<br/>\r\n";
		$text = $prolog;
		// Großbuchstaben
		//$text = str_replace("#","",$text);
	// ersetzen der Dollarzeichen / finden von mathe-Bereichen:	
		do{
			$posi = strpos($text,"$");
			if ($posi > -1){
				//echo "Pos:".$posi."<br/>";
				$posii = strpos($text,"$",$posi+1) + 1;
				//echo "Pos:".$posii."<br/>";
				$mitte = substr($text, $posi, $posii-$posi);
				/* Sebastians code
				$mitte = substr($text, $posi, $posii);
				//echo "Mitte:".$mitte."<br/>";
				$mitte = substr($mitte,0,$posii-$posi);
				*/

				// alle restliche Zeichen durch Komma trennen
				// alle ' durch succ ersetzen
				$mathe = substr($mitte,1,$posii-$posi-2);
	
				//echo "vorher: ".$mathe."\r\n<br/>";
				//$mathe = str_replace("'","##succ##",$mathe);
				//$mathe = str_replace("`","##succ##",$mathe);
	
				// Klammern
				$mathe = str_replace("{","(",$mathe);
				$mathe = str_replace("[","(",$mathe);
				$mathe = str_replace("}",")",$mathe);
				$mathe = str_replace("]",")",$mathe);
				// why?! " " is replaced with "','" for some reason.. :/
				$mathe = str_replace("'","",$mathe);

				// Zahlen und Zeichen für Hochkommata markieren
				$string = $mathe;
				//$string = str_replace('  ', ' ', preg_replace('/[0-9]/', '##hka##\0##hke##', $string));
				$string = str_replace('  ', ' ', preg_replace('/\(/', '##hka##\0##hke##', $string));
				$string = str_replace('  ', ' ', preg_replace('/\)/', '##hka##\0##hke##', $string));
				$mathe = $string;

				// Mathemodus-Text parsen und dabei Zahlen und LaTeX-Codes besonders behandeln
				$ggg = "";
				$lat = 0;
				$zahlvor = 0;
				$zahlnach = 0;
				$zahl = 0;
				for ($xa = 0; $xa < strlen($mathe); $xa++){
					if ((	$mathe[$xa] == "0" ||
						$mathe[$xa] == "1" || $mathe[$xa] == "2" ||
						$mathe[$xa] == "3" || $mathe[$xa] == "4" ||
						$mathe[$xa] == "5" || $mathe[$xa] == "6" ||
						$mathe[$xa] == "7" || $mathe[$xa] == "8" ||
						$mathe[$xa] == "9")){
						$zahl = 1;
						//Wenn danach noch eine Ziffer kommt...
						if ($mathe[$xa+1] == "0" || $mathe[$xa+1] == "1" || $mathe[$xa+1] == "2" || $mathe[$xa+1] == "3" || $mathe[$xa+1] == "4" || $mathe[$xa+1] == "5" || $mathe[$xa+1] == "6" || $mathe[$xa+1] == "7" || $mathe[$xa+1] == "8" || $mathe[$xa+1] == "9" ){
							$zahlnach = 1;
						}
						else {
							$zahlnach = 0;
						}
						if ($mathe[$xa-1] == "0" || $mathe[$xa-1] == "1" || $mathe[$xa-1] == "2" || $mathe[$xa-1] == "3" || $mathe[$xa-1] == "4" || $mathe[$xa-1] == "5" || $mathe[$xa-1] == "6" || $mathe[$xa-1] == "7" || $mathe[$xa-1] == "8" || $mathe[$xa-1] == "9" ){
							$zahlvor = 1;
						}
						else{
							$zahlvor = 0;
						}
	
					}
					// öffnendes unicode
					else if (($mathe[$xa] == "\\") ){
						$lat = 1;
						$ggg = $ggg.", '";
						$zahl = 0;
					}
					// schliessendes unicode
					else if ($mathe[$xa] == "," || $mathe[$xa] == "#"){
					//else if ($mathe[$xa] == " "){
						if ($lat == 1){
							$ggg = $ggg."'"; 
						}
						$lat = 0;
						$zahl = 0;
					}
					else {
						$zahl = 0;
					}
					if ($lat == 0 && $zahl == 0){
						$ggg = $ggg." ".$mathe[$xa];
					}
					else {
						//$ggg = $ggg."".$mathe[$xa];
					
						if ($zahl == 1){
							if ($zahlvor == 1 && $zahlnach == 0){
								$ggg = $ggg.$mathe[$xa]."', ";
							}
							else if ($zahlvor == 0 && $zahlnach == 1){
								$ggg = $ggg."'".$mathe[$xa];
							}
							else if ($zahlvor == 1 && $zahlnach == 1){
								$ggg = $ggg."".$mathe[$xa];
							}
							else if ($zahlvor == 0 && $zahlnach == 0){
								$ggg = $ggg." '".$mathe[$xa]."', ";
							}
						}
						else {
							$ggg = $ggg."".$mathe[$xa];
						}
					}
				}
				if ($lat == 1){
					$ggg = $ggg."'";
					$lat = 0;
				}
	
				$ggg = str_replace("'\\"," '\\",$ggg);
				//echo "G: ".$ggg."\r\n<br/>";
	
				$mathe = $ggg;
				$ergmathe = $mathe;
				// Hinufügen von '' um klammern und zahlen, sowie ersetzen von ' durch succ
				
				//echo "Mathe: ".$ergmathe."<br/>\r\n";
				$ergmathe = str_replace(","," ",$ergmathe);
				do{
				$ergmathe = str_replace("  "," ",$ergmathe);
				$le = strpos($ergmathe,"  ");
				//echo "leer?".$le; dasdss
				}while(strpos($ergmathe,"  ")>-1);
				
				if($ergmathe[0] == " ") {$ergmathe = substr($ergmathe,1,strlen($ergmathe));}
				if($ergmathe[strlen($ergmathe)-1] == " ") {
					$ergmathe = substr($ergmathe,0,strlen($ergmathe)-1);
				}
				// Großbuchstaben in ''!
				$ergmathe = str_replace('  ',' ', preg_replace("/[A-Z]/", "'".'\0'."'", $ergmathe));

				// jetzt hier noch die Funktionen entsprechend gestallten
				// frac:
				$posend = 0;
				$posanf = strrpos($ergmathe,"'\\frac' # # h k a # # ( # # h k e # #");
				//echo "???????????".$ergmathe."<br/>\r\n";
				if ($posanf > -1){
					//echo "FRAC:".$ergmathe;
					//$ergmathe = str_replace("'\\frac##hka##(##hke##","'\\frac','(',",$ergmathe);
					$ergmathe = str_replace("# # h k a # # ) # # h k e # # # # h k a # # ( # # h k e # #",",',',",$ergmathe);
					//$ergmathe = str_replace("##hka##)##hke##",",') ",$ergmathe);
					//echo "FRAC:".$ergmathe;
				}
				// sqrt:
				$posend = 0;
				$posanf = strrpos($ergmathe,"'\\sqrt' # # h k a # # ( # # h k e # #");
				//echo "???????????".$ergmathe."<br/>\r\n";
				if ($posanf > -1){
					//$ergmathe = str_replace("'\\sqrt##hka##(##hke##","'\\sqrt','(',",$ergmathe);
					$ergmathe = str_replace("# # h k a # # ) # # h k e # # # # h k a # # ( # # h k e # #",",',',",$ergmathe);
					//$ergmathe = str_replace("##hka##)##hke##",",') ",$ergmathe);
				}
	
				//mathbb
				$posend = 0;
				$mathbb = "'\\mathbb' # # h k a # # ( # # h k e # #";
				$posanf = strpos($ergmathe,$mathbb);
				//echo "???????????".$ergmathe."<br/>\r\n";
				if ($posanf > -1){
					$posend = strpos($ergmathe,")",$posanf);
					$posende = strpos($ergmathe,"')'",$posanf);
					if ($posende <$posend && $posende > -1){
						$posend = $posende;
						$posende = 2;
					}
					else {
						$posende = 0;
					}
					$bb = substr($ergmathe, $posanf + strlen($mathbb), $posend-$posanf-strlen($mathbb));
					
					$bb = str_replace(",","",$bb);
					$bb = str_replace("'","",$bb);
					//$bb = str_replace("#","",$bb);
					$bb = str_replace("')'",")",$bb);
					//$bb = str_replace("succ","",$bb);
					$bb = "'\\bb#".$bb."#";
					//echo "BB:".$bb."<br/>";
					$org = substr($ergmathe,$posanf,$posend-$posanf+3+$posende);
					//echo "<br/>XXXXXXXXXXXXXXXxorg:".$org."<br/>";
					//echo "ersatz: ".$bb."<br/>";
					$ergmathe = str_replace($org,$bb,$ergmathe);
					//echo "mathe:".$ergmathe."!<br/>";
				}

				$ergmathe = str_replace(" ",", ",$ergmathe);
				$ergmathe = str_replace("#, #, s, u, c, c, #, #","succ, ",$ergmathe);
	
				$ergmathe = str_replace(", #, #, #, h, k, a, #, #, #, #, h, k, e, #, #","'",$ergmathe);
				$ergmathe = str_replace("#, #, h, k, a, #, #"," '",$ergmathe);
				$ergmathe = str_replace("#, #, h, k, e, #, #","', ",$ergmathe);

				//echo "erg:".$ergmathe."<br/><br/><br/>";
				$ergmathe = str_replace("', (, '","'('",$ergmathe);
				$ergmathe = str_replace("', ), '","')'",$ergmathe);
				$ergmathe = str_replace(", ,",",",$ergmathe);
				$ergmathe = str_replace(",,",",",$ergmathe);
				$ergmathe = str_replace("''","'",$ergmathe);
				$ergmathe = str_replace(" ","",$ergmathe);
				$ergmathe = str_replace("_","'_'",$ergmathe);
	
				$text = str_replace($mitte,"math([".$ergmathe."])",$text);
				$text = str_replace(", ]","]",$text);
				//echo "erg:".$text."<br/><br/><br/>";
			}
	
		}while($posi > -1);
		
		//$text = $prolog;
		// Rückumwandlung der Konstanten bzw. dummydaten.
		$text = str_replace("#hka####hke##","",$text);
		$text = str_replace("#beg##","",$text);
		$text = str_replace("#e##","end_",$text);
		$text = str_replace("###a##","axiom",$text);
		$text = str_replace("###the##","theorem",$text);
		$text = str_replace("###p##","proof",$text);
		$text = str_replace("###l##","lemma",$text);
		$text = str_replace("###defini##","definition",$text);
		$text = str_replace("#ie#","i.e.",$text);
		$text = str_replace("##komma##","',',",$text);
		
		
		// Hochkommata und anderes durch Umwandlung im Mathemodus
		//$text = str_replace("succ,","",$text);
		$text = str_replace("##succ##","",$text);
		$text = str_replace("#succ##'","",$text);
		//$text = str_replace("''","'",$text);
		$text = str_replace("k,o,m,m,a","','",$text); // mathemodus
		$text = str_replace("'math([","math([",$text);
		$text = str_replace("[',math([","[math([",$text);
		$text = str_replace("])'","])",$text);
		$text = str_replace(",])","])",$text);

		// Bugfix erster Satz: wenn nur ein Satz eingegeben wird, und dieser mit einem oder mehreren leerzeichen beginnt, kein Fehler mehr:
		$sentone = "sentence(1,[','";
		$posone = strrpos($text,$sentone);

		//echo "XXX".$posone;
		while($posone > -1){
			$text = str_replace("sentence(1,[','","sentence(1,['",$text);
			$posone = strrpos($text,$sentone);
		}
		// falls leere Wörter erzeugt werden, lösche sie

		while (strpos($text,",'',") > -1){
			$text = str_replace(",'',",",",$text);
			//echo "<br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/>";
		}
		$text = str_replace("''i.e.''","'i.e.'",$text);


		
		// ersetze alle Großbuchstaben ausserhalb es math. Bereiches durch Kleinbuchstaben
		$text = str_replace("#A#","a",$text);
		$text = str_replace("#B#","b",$text);
		$text = str_replace("#C#","c",$text);
		$text = str_replace("#D#","d",$text);
		$text = str_replace("#E#","e",$text);
		$text = str_replace("#F#","f",$text);
		$text = str_replace("#G#","g",$text);
		$text = str_replace("#H#","h",$text);
		$text = str_replace("#I#","i",$text);
		$text = str_replace("#J#","j",$text);
		$text = str_replace("#K#","k",$text);
		$text = str_replace("#L#","l",$text);
		$text = str_replace("#M#","m",$text);
		$text = str_replace("#N#","n",$text);
		$text = str_replace("#O#","o",$text);
		$text = str_replace("#P#","p",$text);
		$text = str_replace("#Q#","q",$text);
		$text = str_replace("#R#","r",$text);
		$text = str_replace("#S#","s",$text);
		$text = str_replace("#T#","t",$text);
		$text = str_replace("#U#","u",$text);
		$text = str_replace("#V#","v",$text);
		$text = str_replace("#W#","w",$text);
		$text = str_replace("#X#","x",$text);
		$text = str_replace("#Y#","y",$text);
		$text = str_replace("#Z#","z",$text);
		$text = str_replace("#a#","A",$text);
		$text = str_replace("#b#","B",$text);
		$text = str_replace("#c#","C",$text);
		$text = str_replace("#d#","D",$text);
		$text = str_replace("#e#","E",$text);
		$text = str_replace("#f#","F",$text);
		$text = str_replace("#g#","G",$text);
		$text = str_replace("#h#","H",$text);
		$text = str_replace("#i#","I",$text);
		$text = str_replace("#j#","J",$text);
		$text = str_replace("#k#","K",$text);
		$text = str_replace("#l#","L",$text);
		$text = str_replace("#m#","M",$text);
		$text = str_replace("#n#","N",$text);
		$text = str_replace("#o#","O",$text);
		$text = str_replace("#p#","P",$text);
		$text = str_replace("#q#","Q",$text);
		$text = str_replace("#r#","R",$text);
		$text = str_replace("#s#","S",$text);
		$text = str_replace("#t#","T",$text);
		$text = str_replace("#u#","U",$text);
		$text = str_replace("#v#","V",$text);
		$text = str_replace("#w#","W",$text);
		$text = str_replace("#x#","X",$text);
		$text = str_replace("#y#","Y",$text);
		$text = str_replace("#z#","Z",$text);
		$text = str_replace("#,","",$text);
		$text = str_replace("?","'.'",$text);
		//$text = str_replace("#math",",math",$text);
		//$text = str_replace("\r\nmath",",math",$text);
		//$text = str_replace("]),',',',math([","]),',',math([",$text);
		;
		// entstanden durch math.Großbuchstaben
		do {
			$text = str_replace(",#,",",",$text);
		}while(strpos($text,",#,")>-1);
		$text = str_replace(",#","",$text);
		//echo "Fertig:<br/>\r\n".$text."<br/>\r\n";
		$inh = $text;
		// Umwandlung der Satzliste in eine große Prologliste:
		$pllist = str_replace(").","),<br/>",$inh);
		$pllist = "[<br/>".substr($pllist,0,strlen($pllist)-8)."<br/>]";
		
	}
}
// Beispiel in Textfeld einlesen
else {
	$dateiname = $_GET['proof'];	

	//Datei zum lesen öffnen
	if (strlen(trim($dateiname)) > 0){
		$dateiname.=".txt";
		$datei = fopen('../ex/'.$dateiname,'r') or die ("Kann Datei nicht lesen.");
		$test = "";
		while (!feof($datei))	{
			$zeile .= fgets($datei,1024);
			$test = rtrim($zeile)."\r\n";
		}
		$strtmp = $test; 
		//DATEI WIEDER SCHLIESSEN:
		fclose($datei);
	}

}

?>
