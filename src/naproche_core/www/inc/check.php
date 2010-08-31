<?PHP 
$text = $_POST["txtinput"];
$error = "";
$warning = "";
if (isset($text)){
	// Anzahl der Dollar-Zeichen gerade?
	$anzd = strlen($text) - strlen(str_replace("$", "", $text));
	//echo "anzahl $:".$anzd."<br/>";
	$fehler = 0;
	if ($anzd%2==1){
		//echo "anzahl $ ungerade! Eingabefehler im Textfeld!<br/>";
		$error = $error."<br/>Number of $-sign odd! Inputerror in textarea!<br/>";
	}

	else {
		$error = $error."";
	}

	//suche nach \begin{...} und entsprechendes \end{...}
	$beg = "\\begin{";
	$posi = strpos($text,$beg);
		
	if ($posi > -1){
		//echo "Pos:".$posi."<br/>";
		$posii = strpos($text,"}",$posi);
		if ($posii > -1){
			//echo "Pos:".$posii."<br/>";
			//$error = $error."<br/>x<br/>";
			$mitte = substr($text, $posi+strlen($beg));
			$mitte = substr($mitte, 0,strpos($mitte,"}"));
			//$mitte = substr($text, 8, 1);
			//echo "Inhalt: ".$posi+strlen($beg)."|".$posii."|".$mitte."!<br/>";
	
			$posiii = strpos($text,"\\end{".$mitte."}",$posii);
			if ($posiii > -1){
				//$error = $error."<br/>end-element da!<br/>";
			}
			else{
				$error = $error."<br/>begin-element not closed!<br/>";
			}
		}
		else {
			$error = $error."<br/>begin-struktur not closed!<br/>";
		}
	}
}
//Überprüfung von Inhalten nach Punkten
$anzp = strlen($text) - strlen(str_replace(".", "", $text));
$tmptxt = rtrim($text);
$posc = 0;
for ($i = 0; $i < $anzp; $i++) {
	$pos = strpos($tmptxt,".");
	if ($pos > -1){
		//$posc = $posc + $pos + 1;
		$tmptxt = rtrim(substr($tmptxt,$pos+1,strlen($tmptxt)));
	}
	else {
		
	}
}
// ausnahmen, die ignoriert werden können
$tmptxt = str_replace("\\begin{theorem}", "", $tmptxt);
$tmptxt = str_replace("\\begin{lemma}", "", $tmptxt);
$tmptxt = str_replace("\\begin{definition}", "", $tmptxt);
$tmptxt = str_replace("\\begin{axiom}", "", $tmptxt);
$tmptxt = str_replace("\\begin{proof}", "", $tmptxt);
$tmptxt = str_replace("\\end{theorem}", "", $tmptxt);
$tmptxt = str_replace("\\end{lemma}", "", $tmptxt);
$tmptxt = str_replace("\\end{definition}", "", $tmptxt);
$tmptxt = str_replace("\\end{axiom}", "", $tmptxt);
$tmptxt = str_replace("\\end{proof}", "", $tmptxt);
$tmptxt = str_replace("\\", "", $tmptxt); // latex-Zeilenumbruch
$tmptxt = trim($tmptxt);
if (strlen($tmptxt) > 0){
	$warning = "Missing full stop: '".$tmptxt."' not allocated!";
	//echo "Warning: ".$warning."<br>\r\n";
}
?>