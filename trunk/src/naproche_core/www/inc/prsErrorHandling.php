<?PHP
include('../tmp/'.$sesid.'/error.php'); //zum aufruf von php
//include($_SERVER["DOCUMENT_ROOT"].'/naproche/tmp/error.php'); //zum aufruf von php
//echo "Error: ".$prologErrorList;
// durchlaufe die Fehlerliste
$anz = sizeof($prologErrorList);
if ($anz > 0){
echo "<br/><b>Warning(s)/Error(s) occurred!</b><br/><br/>";
?>
<table border="0" rules="rows" width="99%" style="max-width:1024px;">
<tr>
<?
if ($debugMode == 1){
	echo '<td width="15%"><b>Failed predicate</b></td>';
}
?>
<td width="15%"><b>Type</b></td>
<td width="35%"><b>Location</b></td>
<td width="38%"><b>Description</b></td>
</tr>
<?
	//echo "Number of Items of Errorlist: ".$anz."<br/>";
	for($i = 0; $i < $anz; $i++){
		//echo "TEST".$i;
		$error = $prologErrorList[$i][1];
		$plerror = $prologErrorList[$i][2];
		$location = $prologErrorList[$i][3];
		$pldesc = $prologErrorList[$i][4];
		if ($plerror > -1){
			//echo "Error: ".$plerror;
			if ($plerror != "naproche_text"){
				
				if ($prologErrorList[$i][0] == "error"){
					echo "<tr bgcolor='#FF5555'>";
				}
				else if ($prologErrorList[$i][0] == "warning"){
					// echo "<tr bgcolor='#FFFF00'>";
					echo "<tr bgcolor='#FF8800'>";
				}
				else {
					echo "<tr bgcolor='#DDDDDD'>";
				}
				if ($debugMode == 1){
					echo "<td>".$plerror."</td>";
				}
				echo "<td>".$error."</td>";
				echo "<td>".$location."</td>";
				echo "<td>".$pldesc."</td>";
				echo "</tr>";
				//echo "Type of error:  " .$plerror."<br/>";
				//echo "Description:    " .$pldesc."<br/>";
			}
			else {

				echo "<tr bgcolor='#FF5555'>";
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
				$punkt = 0;
				// Wenn der übergebene Wert = 0 war...
				if ($wort == "-1" || $wort == -1){

					$wort = 0;
					$punkt = 1;
				}
				$satz = substr($orginal,$pkt[$id-1],$pkt[$id]-$pkt[$id-1]-1);
				$satz = trim($satz);
				$satz = substr($satz,0,strlen($satz));
				$org = $satz;
				$delta = 0;
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
				
				//echo "?-anzahl: ".$fanz."<br/>";
				//echo "XXXXXSatz:".$satz."!<br/>";
				
				
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

				// Falls der Punkt mitmarkiert werden soll, ist $punkt = 1, sonst 0
				$ende = $ende + $punkt;
				
				// ACHTUNG! start und ende sind so richtig, 
				// zielenumbrüche jeglicher Art müssen 
				// noch rausgerechnet werden!
				echo "<td>".$error."</td>";
				echo "<td>".
				"<span>". $location."&emsp; &rarr; &emsp; </span>".
				"<span id='blue' onclick='findSentence(".$start.",".$ende.")' >".
				"highlight error:".
				//$start.",".$ende.
				"</span>".
				"</td>";

				echo "<td>".$pldesc."</td>";
				echo "</tr>";
			}
		}
	}
?>
</table>
<?
echo "<br/><p>Number of Warning(s)/Error(s): ".$anz."</p><br/>";
}
?>
