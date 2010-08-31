<?php 
error_reporting(E_ALL);
ini_set('display_errors', 1);

include("../inc/init.php");
//echo "DBM:".$debugMode."<br/>";
if ($debugMode != "on"){
	$debugMode = "off";
} 
	
$orginal = $strtmp;
//This script takes the input of the HTML form, writes it into a temp file
//and runs the prolog script on it. 
//echo "prolog:".$prolog."<br/>";
//echo "Inhalt: ".$button."<br>";
if ($button == "Debug-Mode on"){
	$debugMode = "off";
	$pfad = "../";
	include('../inc/htmlhead.php');
	include('../inc/header.php');
	$pfad = "";
	include("../inc/menuInterface.php");
	
	include('../inc/main.php');
	include('../inc/footer.html');
}
else if ($button == "Debug-Mode off"){
	$debugMode = "on";
	$pfad = "../";
	include('../inc/htmlhead.php');
	include('../inc/header.php');
	$pfad = "";
	include("../inc/menuInterface.php");
	
	include('../inc/main.php');
	include('../inc/footer.html');
}

else if ($button == "create PDF"){
	$pfad = "../";
	include('../inc/htmlhead.php');
	//include('../inc/header.php');
	//$pfad = 0;
	//$pfad = "";
	//include("../inc/menuInterface.php");
	include("../inc/pdf.php");
	
	//echo "<div id=\"content\">PDF</div>";
}
else if ($button == "Create PRS"){
	// Create temp file
	system('rm '.$_SERVER["DOCUMENT_ROOT"].'/naproche/tmp/'.$sesid.'/*');
	$tmpfile = tempnam('/naproche/tmp/'.$sesid,'');
	$temp = fopen($tmpfile,'w');
	
	// Schreibe Textfeldinhalt und restliche Werte in tmpfile  Preparsing!
	$werte =        "submit('".$button."').\r\n";
	$werte = $werte."session_id('".$sesid."').\r\n";
	$werte = $werte."check_prover('".$sprover."').\r\n";
	$werte = $werte."check_place('".$plocation."').\r\n";
	$werte = $werte."check_time(".$tpo.").\r\n";
	$werte = $werte."max_distance(".$soutput.").\r\n";
	$werte = $werte."debug('".$debugMode."').\r\n";
	$inh = $inh."\r\n".$werte;
	fwrite($temp,$inh);
	// Aufruf vom Prolog-Modul
	system($_SERVER["DOCUMENT_ROOT"].'/naproche/cgi-bin/process_input.pl '.$tmpfile);
	// Löschen der temporären Datei
	fclose($temp);

	if (file_exists($_SERVER["DOCUMENT_ROOT"].'/naproche/tmp/'.$sesid.'/prsdiv.html')) {
?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
      "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">

<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
    <title>PRS Display</title>
    <link
	href="../css/prs_display.css"
	rel="stylesheet"
	type="text/css"/>
    <meta
	http-equiv="content-type"
	content="application/xhtml+xml; charset=utf-8"/>
  </head>
  <body>
<?
include("../inc/prsErrorHandling.php");
include('../tmp/'.$sesid.'/prsdiv.html'); // bei fehler wird keine leere Datei

?>
</body>
</html>
<? 
	} 
	else {
		$pfad = "../";
		include('../inc/htmlhead.php');
		include('../inc/header.php');
		$pfad = "";
		$error = "Could not create PRS!<br/>";
		include("../inc/menuInterface.php");
	
		include('../inc/main.php');
		include('../tmp/'.$sesid.'/error.php');
	}

}
else if ($button == "Preparse Input"){
	$pfad = "../";
	include('../inc/htmlhead.php');
	include('../inc/header.php');
	$pfad = "";
	include("../inc/menuInterface.php");

	include('../inc/main.php');
	
	//echo "<br/>Inhalt der temporären Datei: <br/>".$inh;
	
	echo "<br/>Prolog readable input: <br/>".$pllist;

	include('../inc/footer.html');
}
// Logical Check gedrückt
else{
	system('rm '.$_SERVER["DOCUMENT_ROOT"].'/naproche/tmp/'.$sesid.'/*');
	include('check.php'); //zum aufruf von php
	$pfad = "../";
	include('../inc/htmlhead.php');
	include('../inc/header.php');
	$pfad = "";
	include("../inc/menuInterface.php");

	include('../inc/main.php');
	echo "<br/>";
	// NEU-TBC: erstellen der Sätze anhand der Satzid:
	// Dateiname: orginal.input
	//echo "Sätze: <br/>";
/*
	$org = "[";
	for($i = 1; $i < sizeof($pkt)-1; $i++){
		$tmp = substr($strtmp,$pkt[$i-1],$pkt[$i]);
		$tmp = str_replace("i.e.","#ie#",$tmp);
		$tmp = substr($tmp,0,strpos($tmp,".")+1);
		$tmp = str_replace("#ie#","i.e.",$tmp);
		$org = $org."org_sentence(".$i.",'".$tmp."'),";
		//echo $i.": ".$tmp."<br/>";
	}
	$org = substr($org,0,strlen($org)-1)."].";
	//echo "org: ".$org."<br/>";	
	// schreibe Datei:
	$tmpfile = $_SERVER["DOCUMENT_ROOT"].'/naproche/tmp/'.$sesid.'/original.input';
	//$tmpfile = '/tmp/'.'sentence.txt';
	$temp = fopen($tmpfile,'w');
	fwrite($temp,$org);
	fclose($temp);
	// ENDE NEU*/

	// Create temp file
	//$tmpfile = tempnam('/naproche/tmp/'.$sesid,'');
	$tmpfile = $_SERVER["DOCUMENT_ROOT"].'/naproche/tmp/'.$sesid.'/preparse.input';
	//$tmpfile = '/tmp/'.'sentence.txt';
	$temp = fopen($tmpfile,'w');
	$tmps = Array();
	for ($i = 0; $i < sizeof($pkt)-2; $i++){
		$ffr = substr($strtmp,$pkt[$i],$pkt[$i+1]-$pkt[$i]);
		//$tmps[$i] = substr($strtmp,$pkt[$i],$pkt[$i+1]-$pkt[$i]);

		do{
			$pos = strpos($ffr,"%");
			$posi = strpos($ffr,"\r\n") + 2;
			if($pos > -1){
				$ggg = substr($ffr,$pos,$posi-$pos);
				//echo "ggg:".$ggg."<br/>";
				$ffr = str_replace($ggg,"",$ffr);
			}
		}while($pos > -1);
		
		$pos = -1;
		do{
			$pos = strpos($ffr,"/*");
			//echo "ggg:".$pos."<br/>";
			$posi = strpos($ffr,"*/") + 2;
			if ($pos > -1){
				$ggg = substr($ffr,$pos,$posi-$pos);
				//echo "ggg:".$ggg."<br/>";
				$ffr = str_replace($ggg,"",$ffr);
				//echo "string:".$ffr."<br/>";
			}
			$j = $j + 1;
		}while($pos > -1);
		$ffr = str_replace("\r\n","",$ffr);
		$ffr = str_replace("\\","!",$ffr);
		$ffr = str_replace("!!","",$ffr);
		$ffr = str_replace("!","\\\\",$ffr);
		$ffr = str_replace("<","&#60;",$ffr);

		$tmps[$i] = $ffr;
		//echo "tmp:".$tmps[$i]."<br/>";
	}
	// Ausgabekosmetik: Satztabelle im div-Tag output öffnen
	if(sizeof($tmps) > 0){echo "<div id = 'output'>";}
	?>
	<script language="JavaScript">
		<?
		// Übertrage die Werte des PHP-Arrays in JS-Variablen
		for ($i = 1; $i < sizeof($tmps)+1; $i++){
			echo 'satz'.$i.' = new sentences("'.$i.'","'.$tmps[$i-1].'","000000"); ';
			echo "satz".$i.".write(); ";
			//echo "document.writeln(satzPos[".$i."]);";
		}
		?>
	</script>
	<?
	// Ausgabekosmetik: Satztabelle im div-Tag output schliessen
	if(sizeof($tmps) > 0){echo "</div>";}

	// Schreibe Textfeldinhalt und restliche Werte in tmpfile  Preparsing!
	$werte =        "submit('".$button."').\r\n";
	$werte = $werte."session_id('".$sesid."').\r\n";
	$werte = $werte."check_prover('".$sprover."').\r\n";
	$werte = $werte."check_place('".$plocation."').\r\n";
	$werte = $werte."check_time(".$tpo.").\r\n";
	$werte = $werte."max_distance(".$soutput.").\r\n";
	$werte = $werte."debug('".$debugMode."').\r\n";
	$inh = $inh."\r\n".$werte;
	fwrite($temp,$inh);
	//echo "Inhalt: <br/>" .$inh."<br/>";
	// Aufruf vom Prolog-Modul
	system($_SERVER["DOCUMENT_ROOT"].'/naproche/cgi-bin/process_input.pl '.$tmpfile);

	// Löschen der temporären Datei
	fclose($temp);
	$inh = str_replace("\r\n","\r\n<br/>",$inh);
	$inh = str_replace("#","<br/>",$inh);

	include("../inc/prsErrorHandling.php");
	include('../inc/footer.html');
}

// durchlaufe die Fehlerliste
include('../naproche/tmp/'.$sesid.'/error.php'); //zum aufruf von php

$fehlerAnzeigen = 1; // 0 für wirksamen unteren kommentar
/*
if(sizeof($prologErrorList)>0){
	for($i = 0; $i < sizeof($prologErrorList); $i++){
		$plerror = $prologErrorList[$i][2];
		if ($plerror == "naproche_text"){
			$fehlerAnzeigen = 1;
		}
	}
}
//*/
if ($fehlerAnzeigen == 1 && $button == "Create PRS"){

//echo "TEST: ".sizeof($prologErrorList);
for($i = 0; $i < sizeof($prologErrorList); $i++){

$plerror = $prologErrorList[$i][2];
$pldesc = $prologErrorList[$i][4];
if ($plerror > -1){
	//echo "Error: ".$plerror;
	$satzid = -1;
	$lastword = -1;
		//echo "Type of error:  " .$plerror."<br/>";
		//echo "Description:    " .$pldesc."<br/>";
	if ($plerror == "naproche_text"){
		$plpos = $prologErrorList[$i][3];
		//echo "<br/>PL: ".$plpos."<br/>";
		$satzid = substr($plpos,1,strpos($plpos,",")-1);
		$lastword = substr($plpos,strpos($plpos,",")+2, strpos($plpos,"]")-strpos($plpos," ")-1);
		//echo "<br/>lastword: ".$lastword."<br/>";
		//echo "<br/>ID: ".$satzid."<br/>Wort: ".$lastword;

		$delta = 0;
		// Markierung der Sätze
		//echo "<br/>SatzID: ".$id."<br/>";
		//echo "Wortnr: ".$wort."<br/>";
		//echo '<img src="../img/icon/kasten.GIF" width="25" height="17" onclick="findSentence('.$pkt[$id-1].','.$pkt[$id].')">';
		$id = $satzid; 
		$wort = $lastword-1; // wort = 0: letztes wort
		$satz = substr($orginal,$pkt[$id-1],$pkt[$id]-$pkt[$id-1]-1);
		$satz = trim($satz);
		$satz = substr($satz,0,strlen($satz));
		$org = $satz;
		$delta = 1;
		//echo "<br/>Satz: ".$satz."<br/>";
		//echo "<br/>";
		for ($i = $wort; $i > 0; $i--){
			//echo $i." Satz:".$satz."<br/>pos: ".strrpos($satz,"$")."/".strlen($satz)."<br/>";
			$pos = strlen($satz);
			//echo "Satz: ".$satz."!<br/>";
			if ((strrpos($satz,"$")+1) == strlen($satz)){
				//echo "$!<br/>";
				$satz = (substr($satz,0,strlen($satz)-1));
				$pos = strrpos($satz,"$")-1;
				$satz = (substr($satz,0,$pos));
				//echo "Satz: ".$satz."!<br/>";
			}
			else if ((strrpos($satz,",")+1) == strlen($satz)){
				//echo "KOMMA!<br/>";
				$satz = (substr($satz,0,strrpos($satz,",")));
				$pos = strrpos($satz,",");
				//$satz = (substr($satz,0,$pos));
			}
			else {
				$pos = strrpos($satz," ");
				$satz = (substr($satz,0,$pos));
				//echo " kein $$$<br/>";
			}
		}
		
		if ((strrpos($satz,"$")+1) == strlen($satz)){
			$pos = strrpos($satz,"$");
			$satz = (substr($satz,0,$pos));
			$pos = strrpos($satz,"$");
			//echo "$".$satz."|".$pos."<br/>"; 
			$satz = (substr($satz,$pos,strlen($satz)))."$";
			//echo "Satz: ".$satz."!<br/>";
		}
		else if ((strrpos($satz,",")+1) == strlen($satz)){
		//	echo "KOMMA!<br/>";
			$satz = (substr($satz,0,strlen($satz)-1));
			$pos = strrpos($satz," ");
			//$satz = (substr($satz,0,$pos));
		}
		else {
			$pos = strrpos($satz," ");
			$satz = (substr($satz,$pos,strlen($satz)));
		}
		
		// Anpassung der Markierung durch die Komma
		if ($pos == 0 && $satz[0] != " ") $delta = 0;
		$start = $pkt[$id-1] + $pos + $delta;
		$start = $start +1; // bei \r\n zwei dazu, sonst nicht. wie auch immer das gehen soll
		$ende = $start + strlen($satz);
		$satz = substr($strtmp,$start,$ende-$start);

		$fanz = substr_count($satz, "\r");
		$fanz = $fanz + substr_count($satz, "\n");
				
		$poskomma = strpos($satz,",");
		$posleer = strpos($satz," ");
		$posdollar = strpos($satz,"$");
		if (strlen($satz) > 1 && $poskomma == strlen($satz)-1){
			$ende = $ende - 1;
		}
		else if (strlen($satz) > 1 && $poskomma < $posleer && $poskomma > -1){
			$start = $start + $poskomma;
			$ende = $start + 1;
		}
		if ($posdollar > 0){
			//echo "$";
			$start = $start -1;
		}
	}
}// ende FOR (Fehlerliste)

	$anz = sizeof($prologErrorList);

	if ($anz > 0){
	echo "<br/>";
	//echo "<br/><b>Error occurred!</b><br/><br/>";
	?>
	<div id="output"><br/>
	Warning(s)/Error(s) occurred!
	<br/>
	<table border="0" rules="rows" width="99%" style="max-width:1024px;">
	<tr>
<?
if ($debugMode == 1){
	echo '<td width="20%"><b>Failed predicate</b></td>';
}
?>
	<td width="15%"><b>Type of error</b></td>
	<td width="25%"><b>Location</b></td>
	<td width="40%"><b>Description</b></td>
	</tr>
	<?
		for($i = 0; $i < $anz; $i++){
			
			$error = $prologErrorList[$i][1];
			$plerror = $prologErrorList[$i][2];
			$location = $prologErrorList[$i][3];
			$pldesc = $prologErrorList[$i][4];

			// Farbgebung
			if ($prologErrorList[$i][0] == "error"){
				echo "<tr bgcolor='#FF5555'>";
			}
			else if ($prologErrorList[$i][0] == "warning"){
				echo "<tr bgcolor='#FFFF00'>";
			}
			else {
				echo "<tr bgcolor='#DDDDDD'>";
			}

			if ($debugMode == 1){
				echo "<td>".$plerror."</td>";
			}
			echo "<td>".$error."</td>";

			// möglicher Link zum highlighten einfügen
			if ($plerror != "naproche_text"){
				echo "<td>".$location."</td>";
			}
			else {
				echo "<td>".
				"<span>". $location."&emsp; &rarr; &emsp; </span>".
				"<span id='blue' onclick='findSentence(".$start.",".$ende.",".$id.")' >".
				"highlight error".
				"</span>".
				"</td>";
				}
			echo "<td>".$pldesc."</td>";
			echo "</tr>";
				//echo "Type of error:  " .$plerror."<br/>";
				//echo "Description:    " .$pldesc."<br/>";
			
		}// for
	?>
	</table>
	<br/><p>Number of Warning(s)/Error(s): <?echo $anz;?></p>
	</div>
	<br/>
	</body>
	</html>
	
	<?
}// keine Prologfehler
	}// 	if ($anz > 0){
}// Ende Fehlervorhanden-fall
//*/
?>
