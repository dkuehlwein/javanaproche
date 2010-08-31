<?php
/* Einbindung der Datei init.php
Variablen:
$strtmp: String mit Inhalt aus dem Textfeld
$prolog: Textinhalt auf Prolog angepasst
*/  
include('../inc/init.php'); //zum aufruf von php
$pfad = "../";
include('../inc/header.php');
$pfad = "";
include('../inc/menuInterface.php');
include('../inc/main.php');
unlink($_SERVER["DOCUMENT_ROOT"]."/naproche/tmp/".$ses."/".$ses.".pdf");
?>
<?php
if (strlen(trim($strtmp)) > 0){
?>
	<br/>
	<p>Creating PDF:</p>
	<?php
		$ses = session_id();
		
		//$datei = fopen("../inc/".$ses.".tex","w");
		$datei = fopen($_SERVER["DOCUMENT_ROOT"]."/naproche/tmp/".$ses."/".$ses.".tex","w");
		//fopen($_SERVER["DOCUMENT_ROOT"]."/naproche/tmp/narx.tex","w");

		// Anfang und Ende für latex  
		$strtmp ="\\documentclass[a4paper]{report}
		\\usepackage{amsmath}
		\\usepackage{amsthm}
		\\usepackage{amssymb}

		\\newtheorem{theorem}{Theorem}[section]
		\\newtheorem{lemma}[theorem]{Lemma}
		\\newtheorem{proposition}[theorem]{Proposition}
		\\newtheorem{corollary}[theorem]{Corollary}		\\newtheorem{axiom}[theorem]{Axiom}
		
		\\newenvironment{definition}[1][Definition]{\\begin{trivlist}
		\\item[\\hskip \\labelsep {\\bfseries #1}]}{\\end{trivlist}}
		\\newenvironment{example}[1][Example]{\\begin{trivlist}
		\\item[\\hskip \\labelsep {\\bfseries #1}]}{\\end{trivlist}}
		\\newenvironment{remark}[1][Remark]{\\begin{trivlist}
		\\item[\\hskip \\labelsep {\\bfseries #1}]}{\\end{trivlist}}
		
		\\title{Naproche}\\begin{document}".$strtmp;
		$strtmp.="\\end{document}";

		// Datei schreiben	
		rewind($datei);
		$inh = trim($strtmp);
		fwrite($datei, $inh);
		fclose($datei);

		// wechseln des Pfades zur Ausführung der latex-Befehle
		chdir("../tmp/".$sesid);
		unlink($_SERVER["DOCUMENT_ROOT"]."/naproche/tmp/".$ses."/".$ses.".pdf");

		// Befehle zum Umwandeln ins PDF-Format
system("latex ".$_SERVER["DOCUMENT_ROOT"]."/naproche/tmp/".$ses."/".$ses.".tex");

if (file_exists($_SERVER["DOCUMENT_ROOT"]."/naproche/tmp/".$ses."/".$ses.".dvi")) {
	//echo "DVI-Datei da! <br/>";
	system("dvipdf ".$_SERVER["DOCUMENT_ROOT"]."/naproche/tmp/".$ses."/".$ses.".dvi");
		// Setzen des Links
	$fp = fopen($_SERVER["DOCUMENT_ROOT"]."/naproche/tmp/".$ses."/".$ses.".pdf","r");

	if (!$fp) {
		echo "<br/><p>Cannot create PDF- no valid data in textfield.</p><br/>";
	}
	else{
		?>
		<br/>
		<br/><div id="green">PDF created: <a href="../tmp/<?php echo $ses."/".$ses?>.pdf">show PDF</a></div> 
		<br/><br/>
		<?PHP
	}
	fclose($fp);
}
else{
	echo "<br/><br/><br/><div id='red'>Cannot create PDF- no valid data in textfield.</div><br/><br/> ";
}


		system("dvipdf ".$_SERVER["DOCUMENT_ROOT"]."/naproche/tmp/".$ses."/".$ses.".dvi");
		
		// lösche alle erzeugten pdf-Dateien aus dem tmp-Verzeichnis 
		unlink ($_SERVER["DOCUMENT_ROOT"]."/naproche/tmp/".$ses."/".$ses.".tex");
		unlink ($_SERVER["DOCUMENT_ROOT"]."/naproche/tmp/".$ses."/".$ses.".dvi");
		unlink ($_SERVER["DOCUMENT_ROOT"]."/naproche/tmp/".$ses."/".$ses.".aux");
		unlink ($_SERVER["DOCUMENT_ROOT"]."/naproche/tmp/".$ses."/".$ses.".log");
		//unlink ($_SERVER["DOCUMENT_ROOT"].$ses.".pdf");
		

	}
	else {?>
		Cannot create PDF- no valid data in textfield. <br/> 
		<br/><br/><br/>
	<?}
	?>