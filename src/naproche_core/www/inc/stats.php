<?php

// showStats: berechnet alle Statistiken über alle Obligationen
// stats: array mit Statistiken zur jeweiligen Obligation
// Rückgabewert:NA
function showStats($stats){
?>
<div id="content">
<h2>statistical analyses:</h2>
<?
$ar = $stats;

// Variablen für Statistiken:
$sum = 0;
$ysum = 0;
$nsum = 0;
$anz = 0;
$yanz = 0;
$nanz = 0;
$dist = 0;
$ydist = 0;
$ndist = 0;
$ymin = 999999;
$nmin = 999999;
$ybmin = 999999;
$nbmin = 999999;
$yumin = 999999;
$numin = 999999;
$ybmax = 0;
$nbmax = 0;
$ymax = 0;
$nmax = 0;
$ypra = 0;
$npra = 0;
$pra = 0;
$yanzdist = 0;
$nanzdist = 0;
$ybanz = 0;
$nbanz = 0;
$ybsum = 0;
$nbsum = 0;
$yuanz = 0;
$nuanz = 0;
$yusum = 0;
$nusum = 0;
$alles = 0;
$allesY = 0;
$allesN = 0;
$mTi = 0;
$mNi = 0;

// große Schliefe
for($j = 0; $j < sizeof($ar); $j++) {

	// überprüfung theorem/no-proof:
	if($ar[$j][1] == "theorem"){
		//echo "Theo<br/>";
		$yanz++;
		$ypra = $ypra + sizeof($ar[$j][2]);
		for($i = 0; $i < sizeof($ar[$j][2]); $i++) {
			$ydist = $ar[$j][2][$i][2];
			$medianTheorem[$mTi] = $ydist;
			$median[$alles] = $ydist;
			$alles  = $alles + 1;
			$mTi  = $mTi + 1;
			$ysum = $ysum + $ydist;
			$yanzdist++;
			if ($ymin > $ydist){
				$ymin = $ydist;
				//echo "min<br/>";
			}
			if ($ymax < $ydist){
				$ymax = $ydist;
				//echo "max<br/>";
			}
			// Anzahl benutzer Prämissen
			if($ar[$j][2][$i][1]=="yes"){
				$ybdist = $ar[$j][2][$i][2];
				$medianTheoremY[$ybanz] = $ybdist;
				$medianY[$allesY] = $ybdist;
				$allesY  = $allesY + 1;
				//echo "yes-distanz prä[".$j."]: ".$ydist."<br/>";
				$ybsum = $ybsum + $ybdist;
				$ybanz++;
				if ($ybmin > $ybdist){
					$ybmin = $ybdist;
					//echo "min<br/>";
				}
				if ($ybmax < $ybdist){
					$ybmax = $ybdist;
					//echo "max<br/>";
				}
			}
			else if($ar[$j][2][$i][1]=="no"){
				$yudist = $ar[$j][2][$i][2];
				$medianTheoremN[$yuanz] = $yudist;
				$medianN[$allesN] = $yudist;
				$allesN = $allesN + 1;
				//echo "yes-distanz prä[".$j."]: ".$ydist."<br/>";
				$yusum = $yusum + $yudist;
				$yuanz++;
				if ($yumin > $yudist){
					$yumin = $yudist;
					//echo "min<br/>";
				}
				if ($yumax < $yudist){
					$yumax = $yudist;
					//echo "max<br/>";
				}
			}
		}
	}
	else if($ar[$j][1] == "noproof"){
		//echo "Neo<br/>";
		$nanz++;
		$npra = $npra + sizeof($ar[$j][2]);
		for($i = 0; $i < sizeof($ar[$j][2]); $i++) {
			$ndist = $ar[$j][2][$i][2];
			$medianNP[$mNi] = $ndist;
			$median[$alles] = $ndist;
			$alles  = $alles + 1;
			$mNi = $mNi + 1;
			$nsum = $nsum + $ndist;
			$nanzdist++;
			if ($nmin > $ndist){
				$nmin = $ndist;
				//echo "min<br/>";
			}
			if ($nmax < $ndist){
				$nmax = $ndist;
				//echo "max<br/>";
			}
			if($ar[$j][2][$i][1]=="yes"){
				$nbdist = $ar[$j][2][$i][2];
				$medianNPY[$nbanz] = $nbdist;
				$medianY[$allesY] = $nbdist;
				$allesY  = $allesY + 1;
				//echo "yes-distanz prä[".$j."]: ".$ydist."<br/>";
				$nbsum = $nbsum + $nbdist;
				$nbanz++;
				if ($nbmin > $nbdist){
					$nbmin = $nbdist;
					//echo "min<br/>";
				}
				if ($nbmax < $nbdist){
					$nbmax = $nbdist;
					//echo "max<br/>";
				}
			}
			else if($ar[$j][2][$i][1]=="no"){
				$nudist = $ar[$j][2][$i][2];
				$medianNPN[$nuanz] = $nudist;
				$medianN[$allesN] = $nudist;
				$allesN = $allesN + 1;
				//echo "yes-distanz prä[".$j."]: ".$ydist."<br/>";
				$nusum = $nusum + $nudist;
				$nuanz++;
				if ($numin > $nudist){
					$numin = $nudist;
					//echo "min<br/>";
				}
				if ($numax < $nudist){
					$numax = $nudist;
					//echo "max<br/>";
				}
			}
		}
	}
}
$anz = $yanz + $nanz;
$pra = $ypra + $npra;
$anzdist = $yanzdist + $nanzdist;
$sum = $ysum + $nsum;
if ($nmin > $ymin){$min = $ymin;}
else {$min = $nmin;}

if ($nbmin > $ybmin){$bmin = $ybmin;}
else {$bmin = $nbmin;}

if ($numin > $yumin){$umin = $yumin;}
else {$umin = $numin;}

if ($nbmax < $ybmax){$bmax = $ybmax;}
else {$bmax = $nbmax;}

if ($numax < $yumax){$umax = $yumax;}
else {$umax = $numax;}

if ($nmax < $ymax){$max = $ymax;}
else {$max = $nmax;}

if ($min == 999999) $min = "N/A";
if ($ymin == 999999) $ymin = "N/A";
if ($nmin == 999999) $nmin = "N/A";
if ($bmin == 999999) $bmin = "N/A";
if ($umin == 999999) $umin = "N/A";
if ($yumin == 999999) $yumin = "N/A";
if ($numin == 999999) $numin = "N/A";
if ($ybmin == 999999) $ybmin = "N/A";
if ($nbmin == 999999) $nbmin = "N/A";

if ($max == 0) $max = "N/A";
if ($ymax == 0) $ymax = "N/A";
if ($nmax == 0) $nmax = "N/A";
if ($bmax == 0) $bmax = "N/A";
if ($umax == 0) $umax = "N/A";
if ($ybmax == 0) $ybmax = "N/A";
if ($yumax == 0) $yumax = "N/A";
if ($nbmax == 0) $nbmax = "N/A";
if ($numax == 0) $numax = "N/A";

$bsum = $ybsum + $nbsum;
$banz = $ybanz + $nbanz;
$usum = $yusum + $nusum;
$uanz = $yuanz + $nuanz;

// Abstand der Tabelleneinträge
$abstand = "20px";

// Ausgabe der berechneten Werte in einer Tabelle
?>
<br/>
<table border="0" cellspacing="0" cellpadding="0">
	<tr>
		<td><h3>general stats</h3></td>
		<td width="<?echo $abstand;?>"></td>
		<td bgcolor='#DDDDDD'>
			total	
		</td>
		<td width="<?echo $abstand;?>"></td>
		<td>
			theorem		
		</td>
		<td width="<?echo $abstand;?>"></td>
		<td bgcolor='#DDDDDD'>
			no proof		
		</td>
	</tr>
	<tr><td><hr/></td><td><hr/></td><td bgcolor='#DDDDDD'><hr/></td><td ><hr/></td><td><hr/></td><td><hr/></td><td bgcolor='#DDDDDD'><hr/></td></tr>
	<tr>
		<td>number of obligations</td>
		<td width="<?echo $abstand;?>"></td>
		<td bgcolor='#DDDDDD'>
			<?php echo $anz;?>	
		</td>
		<td width="<?echo $abstand;?>"></td>
		<td>
			<?php echo $yanz;?>	
		</td>
		<td width="<?echo $abstand;?>"></td>
		<td bgcolor='#DDDDDD'>
			<?php echo $nanz;?>	
		</td>
	</tr>
	<tr>
		<td>number of all premises in all obligations</td>
		<td width="<?echo $abstand;?>"></td>
		<td bgcolor='#DDDDDD'>
			<?php echo $pra;?>
		</td>
		<td width="<?echo $abstand;?>"></td>
		<td>
			<?php echo $ypra;?>
		</td>
		<td width="<?echo $abstand;?>"></td>
		<td bgcolor='#DDDDDD'>
			<?php echo $npra;?>
		</td>
	</tr>
	<tr><td><hr/></td><td><hr/></td><td bgcolor='#DDDDDD'><hr/></td><td ><hr/></td><td><hr/></td><td><hr/></td><td bgcolor='#DDDDDD'><hr/></td></tr>
	<tr>
		<td>average number of premises per obligation</td>
		<td width="<?echo $abstand;?>"></td>
		<td bgcolor='#DDDDDD'>
			<?php
				if ($anz > 0) echo round($pra/$anz,2);
				else echo "N/A";
			?>
		</td>
		<td width="<?echo $abstand;?>"></td>
		<td>
			<?php 
				if ($yanz > 0) echo round($ypra/$yanz,2);
				else echo "N/A";
			?>
		</td>
		<td width="<?echo $abstand;?>"></td>
		<td bgcolor='#DDDDDD'>
			<?php
				if ($nanz > 0) echo round($npra/$nanz,2);
				else echo "N/A";
			?>
		</td>
	</tr>
	<tr>
		<td>average number of used premises per obligation</td>
		<td width="<?echo $abstand;?>"></td>
		<td bgcolor='#DDDDDD'>
			<?php
				if ($anz > 0) echo round($banz/$anz,2);
				else echo "N/A";
			?>
		</td>
		<td width="<?echo $abstand;?>"></td>
		<td>
			<?php 
				if ($yanz > 0) echo round($ybanz/$yanz,2);
				else echo "N/A";
			?>
		</td>
		<td width="<?echo $abstand;?>"></td>
		<td bgcolor='#DDDDDD'>
			<?php
				if ($nanz > 0) echo round($nbanz/$nanz,2);
				else echo "N/A";
			?>
		</td>
	</tr>
	<tr>
		<td>average number of non-used premises per obligation</td>
		<td width="<?echo $abstand;?>"></td>
		<td bgcolor='#DDDDDD'>
			<?php
				if ($anz > 0) echo round($uanz/$anz,2);
				else echo "N/A";
			?>
		</td>
		<td width="<?echo $abstand;?>"></td>
		<td>
			<?php 
				if ($yanz > 0) echo round($yuanz/$yanz,2);
				else echo "N/A";
			?>
		</td>
		<td width="<?echo $abstand;?>"></td>
		<td bgcolor='#DDDDDD'>
			<?php
				if ($nanz > 0) echo round($nuanz/$nanz,2);
				else echo "N/A";
			?>
		</td>
	</tr>
	<tr><td><hr/></td><td><hr/></td><td bgcolor='#DDDDDD'><hr/></td><td ><hr/></td><td><hr/></td><td><hr/></td><td bgcolor='#DDDDDD'><hr/></td></tr>
	<tr>
		<td>average distance of premises</td>
		<td width="<?echo $abstand;?>"></td>
		<td bgcolor='#DDDDDD'>
			<?php
				if ($anzdist > 0) echo round($sum/$anzdist,2);
				else echo "N/A";
			?>	
		</td>
		<td width="<?echo $abstand;?>"></td>
		<td>
			<?php
				if ($yanzdist > 0) echo round($ysum/$yanzdist,2);
				else echo "N/A";
			?>			
		</td>
		<td width="<?echo $abstand;?>"></td>
		<td bgcolor='#DDDDDD'>
			<?php
				if ($nanzdist > 0) echo round($nsum/$nanzdist,2);
				else echo "N/A";
			?>	
		</td>
	</tr>
	<tr>
		<td>average distance of used premises</td>
		<td width="<?echo $abstand;?>"></td>
		<td bgcolor='#DDDDDD'>
			<?php
				if ($banz > 0) echo round($bsum/$banz,2);
				else echo "N/A";
			?>
		</td>
		<td width="<?echo $abstand;?>"></td>
		<td>
			<?php
				if ($ybanz > 0) echo round($ybsum/$ybanz,2);
				else echo "N/A";
			?>
		</td>
		<td width="<?echo $abstand;?>"></td>
		<td bgcolor='#DDDDDD'>
			<?php
				if ($nbanz > 0) echo round($nbsum/$nbanz,2);
				else echo "N/A";
			?>
		</td>
	</tr>
	<tr>
		<td>average distance of non-used premises</td>
		<td width="<?echo $abstand;?>"></td>
		<td bgcolor='#DDDDDD'>
			<?php
				if ($uanz > 0) echo round($usum/$uanz,2);
				else echo "N/A";
			?>
		</td>
		<td width="<?echo $abstand;?>"></td>
		<td>
			<?php
				if ($yuanz > 0) echo round($yusum/$yuanz,2);
				else echo "N/A";
			?>
		</td>
		<td width="<?echo $abstand;?>"></td>
		<td bgcolor='#DDDDDD'>
			<?php
				if ($nuanz > 0) echo round($nusum/$nuanz,2);
				else echo "N/A";
			?>
		</td>
	</tr>
	<tr><td><hr/></td><td><hr/></td><td bgcolor='#DDDDDD'><hr/></td><td ><hr/></td><td><hr/></td><td><hr/></td><td bgcolor='#DDDDDD'><hr/></td></tr>
	<tr>
		<td>median distance of obligation</td>
		<td width="<?echo $abstand;?>"></td>
		<td bgcolor='#DDDDDD'>
			<?php echo getMedian($median);?>
		</td>
		<td width="<?echo $abstand;?>"></td>
		<td>
			<?php echo getMedian($medianTheorem);?>
		</td>
		<td width="<?echo $abstand;?>"></td>
		<td bgcolor='#DDDDDD'>
			<?php echo getMedian($medianNP);?>
		</td>
	</tr>
	<tr>
		<td>median distance of used obligation</td>
		<td width="<?echo $abstand;?>"></td>
		<td bgcolor='#DDDDDD'>
			<?php echo getMedian($medianY);?>
		</td>
		<td width="<?echo $abstand;?>"></td>
		<td>
			<?php echo getMedian($medianTheoremY);?>
		</td>
		<td width="<?echo $abstand;?>"></td>
		<td bgcolor='#DDDDDD'>
			<?php echo getMedian($medianNPY);?>
		</td>
	</tr>
	<tr>
		<td>median distance of non-used obligation</td>
		<td width="<?echo $abstand;?>"></td>
		<td bgcolor='#DDDDDD'>
			<?php echo getMedian($medianN);?>
		</td>
		<td width="<?echo $abstand;?>"></td>
		<td>
			<?php echo getMedian($medianTheoremN);?>
		</td>
		<td width="<?echo $abstand;?>"></td>
		<td bgcolor='#DDDDDD'>
			<?php echo getMedian($medianNPN);?>
		</td>
	</tr>
	<tr><td><hr/></td><td><hr/></td><td bgcolor='#DDDDDD'><hr/></td><td ><hr/></td><td><hr/></td><td><hr/></td><td bgcolor='#DDDDDD'><hr/></td></tr>
	<tr>
		<td>minimal distance of obligations</td>
		<td width="<?echo $abstand;?>"></td>
		<td bgcolor='#DDDDDD'>
			<?php echo $min;?>
		</td>
		<td width="<?echo $abstand;?>"></td>
		<td>
			<?php echo $ymin;?>
		</td>
		<td width="<?echo $abstand;?>"></td>
		<td bgcolor='#DDDDDD'>
			<?php echo $nmin;?>
		</td>
	</tr>
	<tr>
		<td>minimal distance of used obligations</td>
		<td width="<?echo $abstand;?>"></td>
		<td bgcolor='#DDDDDD'>
			<?php echo $bmin;?>
		</td>
		<td width="<?echo $abstand;?>"></td>
		<td>
			<?php echo $ybmin;?>
		</td>
		<td width="<?echo $abstand;?>"></td>
		<td bgcolor='#DDDDDD'>
			<?php echo $nbmin;?>
		</td>
	</tr>
	<tr>
		<td>minimal distance of non-used obligations</td>
		<td width="<?echo $abstand;?>"></td>
		<td bgcolor='#DDDDDD'>
			<?php echo $umin;?>
		</td>
		<td width="<?echo $abstand;?>"></td>
		<td>
			<?php echo $yumin;?>
		</td>
		<td width="<?echo $abstand;?>"></td>
		<td bgcolor='#DDDDDD'>
			<?php echo $numin;?>
		</td>
	</tr>
	<tr><td><hr/></td><td><hr/></td><td bgcolor='#DDDDDD'><hr/></td><td ><hr/></td><td><hr/></td><td><hr/></td><td bgcolor='#DDDDDD'><hr/></td></tr>
	<tr>
		<td>maximal distance of obligations</td>
		<td width="<?echo $abstand;?>"></td>
		<td bgcolor='#DDDDDD'>
			<?php echo $max;?>
		</td>
		<td width="<?echo $abstand;?>"></td>
		<td>
			<?php echo $ymax;?>
		</td>
		<td width="<?echo $abstand;?>"></td>
		<td bgcolor='#DDDDDD'>
			<?php echo $nmax;?>
		</td>
	</tr>
	<tr>
		<td>maximal distance of used obligations</td>
		<td width="<?echo $abstand;?>"></td>
		<td bgcolor='#DDDDDD'>
			<?php echo $bmax;?>
		</td>
		<td width="<?echo $abstand;?>"></td>
		<td>
			<?php echo $ybmax;?>
		</td>
		<td width="<?echo $abstand;?>"></td>
		<td bgcolor='#DDDDDD'>
			<?php echo $nbmax;?>
		</td>
	</tr>
	<tr>
		<td>maximal distance of non-used obligations</td>
		<td width="<?echo $abstand;?>"></td>
		<td bgcolor='#DDDDDD'>
			<?php echo $umax;?>
		</td>
		<td width="<?echo $abstand;?>"></td>
		<td>
			<?php echo $yumax;?>
		</td>
		<td width="<?echo $abstand;?>"></td>
		<td bgcolor='#DDDDDD'>
			<?php echo $numax;?>
		</td>
	</tr>
	
</table>
<br/>
<?
}
?>

<?php
// compareStats: berechnet statistiken zu einer ausgewählten Obligation, Vergleicht diese mit Sollwerten aus einer Datei und zeigt alles anschliessend mit der Information an, wieviele Fehler gefunden wurden.
// stats: array mit Statistiken zur jeweiligen Obligation
// dname: Dateiname mit den erwarteten Ergebnissen.
// Rückgabewert:NA
function compareStats($stats,$dname){
include($dname);
?>
<div id="content">
<h2>statistical analyses:</h2>
<?
$ar = $stats;

// Variablen für Statistiken:
$sum = 0;
$ysum = 0;
$nsum = 0;
$anz = 0;
$yanz = 0;
$nanz = 0;
$dist = 0;
$ydist = 0;
$ndist = 0;
$ymin = 999999;
$nmin = 999999;
$ybmin = 999999;
$nbmin = 999999;
$yumin = 999999;
$numin = 999999;
$ybmax = 0;
$nbmax = 0;
$ymax = 0;
$nmax = 0;
$ypra = 0;
$npra = 0;
$pra = 0;
$yanzdist = 0;
$nanzdist = 0;
$ybanz = 0;
$nbanz = 0;
$ybsum = 0;
$nbsum = 0;
$yuanz = 0;
$nuanz = 0;
$yusum = 0;
$nusum = 0;

// große Schliefe
for($j = 0; $j < sizeof($ar); $j++) {

	// überprüfung theorem/no-proof:
	if($ar[$j][1] == "theorem"){
		//echo "Theo<br/>";
		$yanz++;
		$ypra = $ypra + sizeof($ar[$j][2]);
		for($i = 0; $i < sizeof($ar[$j][2]); $i++) {
			$ydist = $ar[$j][2][$i][2];
			$ysum = $ysum + $ydist;
			$yanzdist++;
			if ($ymin > $ydist){
				$ymin = $ydist;
				//echo "min<br/>";
			}
			if ($ymax < $ydist){
				$ymax = $ydist;
				//echo "max<br/>";
			}
			// Anzahl benutzer Prämissen
			if($ar[$j][2][$i][1]=="yes"){
				$ybdist = $ar[$j][2][$i][2];
				//echo "yes-distanz prä[".$j."]: ".$ydist."<br/>";
				$ybsum = $ybsum + $ybdist;
				$ybanz++;
				if ($ybmin > $ybdist){
					$ybmin = $ybdist;
					//echo "min<br/>";
				}
				if ($ybmax < $ybdist){
					$ybmax = $ybdist;
					//echo "max<br/>";
				}
			}
			else if($ar[$j][2][$i][1]=="no"){
				$yudist = $ar[$j][2][$i][2];
				//echo "yes-distanz prä[".$j."]: ".$ydist."<br/>";
				$yusum = $yusum + $yudist;
				$yuanz++;
				if ($yumin > $yudist){
					$yumin = $yudist;
					//echo "min<br/>";
				}
				if ($yumax < $yudist){
					$yumax = $yudist;
					//echo "max<br/>";
				}
			}
		}
	}
	else if($ar[$j][1] == "noproof"){
		//echo "Neo<br/>";
		$nanz++;
		$npra = $npra + sizeof($ar[$j][2]);
		for($i = 0; $i < sizeof($ar[$j][2]); $i++) {
			$ndist = $ar[$j][2][$i][2];
			$nsum = $nsum + $ndist;
			$nanzdist++;
			if ($nmin > $ndist){
				$nmin = $ndist;
				//echo "min<br/>";
			}
			if ($nmax < $ndist){
				$nmax = $ndist;
				//echo "max<br/>";
			}
			if($ar[$j][2][$i][1]=="yes"){
				$nbdist = $ar[$j][2][$i][2];
				//echo "yes-distanz prä[".$j."]: ".$ydist."<br/>";
				$nbsum = $nbsum + $nbdist;
				$nbanz++;
				if ($nbmin > $nbdist){
					$nbmin = $nbdist;
					//echo "min<br/>";
				}
				if ($nbmax < $nbdist){
					$nbmax = $nbdist;
					//echo "max<br/>";
				}
			}
			else if($ar[$j][2][$i][1]=="no"){
				$nudist = $ar[$j][2][$i][2];
				//echo "yes-distanz prä[".$j."]: ".$ydist."<br/>";
				$nusum = $nusum + $nudist;
				$nuanz++;
				if ($numin > $nudist){
					$numin = $nudist;
					//echo "min<br/>";
				}
				if ($numax < $nudist){
					$numax = $nudist;
					//echo "max<br/>";
				}
			}
		}
	}
}
$anz = $yanz + $nanz;
$pra = $ypra + $npra;
$anzdist = $yanzdist + $nanzdist;
$sum = $ysum + $nsum;
if ($nmin > $ymin){$min = $ymin;}
else {$min = $nmin;}

if ($nbmin > $ybmin){$bmin = $ybmin;}
else {$bmin = $nbmin;}

if ($numin > $yumin){$umin = $yumin;}
else {$umin = $numin;}

if ($nbmax < $ybmax){$bmax = $ybmax;}
else {$bmax = $nbmax;}

if ($numax < $yumax){$umax = $yumax;}
else {$umax = $numax;}

if ($nmax < $ymax){$max = $ymax;}
else {$max = $nmax;}

if ($min == 999999) $min = "N/A";
if ($ymin == 999999) $ymin = "N/A";
if ($nmin == 999999) $nmin = "N/A";
if ($bmin == 999999) $bmin = "N/A";
if ($umin == 999999) $umin = "N/A";
if ($yumin == 999999) $yumin = "N/A";
if ($numin == 999999) $numin = "N/A";
if ($ybmin == 999999) $ybmin = "N/A";
if ($nbmin == 999999) $nbmin = "N/A";

if ($max == 0) $max = "N/A";
if ($ymax == 0) $ymax = "N/A";
if ($nmax == 0) $nmax = "N/A";
if ($bmax == 0) $bmax = "N/A";
if ($umax == 0) $umax = "N/A";
if ($ybmax == 0) $ybmax = "N/A";
if ($yumax == 0) $yumax = "N/A";
if ($nbmax == 0) $nbmax = "N/A";
if ($numax == 0) $numax = "N/A";

$bsum = $ybsum + $nbsum;
$banz = $ybanz + $nbanz;
$usum = $yusum + $nusum;
$uanz = $yuanz + $nuanz;
// Ausgabe der berechneten Werte in einer Tabelle
// Bestimmung eines Fehlers

$fehler = 0;
?>
<br/>
<table border="0" rules="rows">
	<tr>
		<td><h3>general stats rot</h3></td>
		<td>
			<p> <font bgcolor='#FF0000'>total</font></p>		
		</td>
		<td>
			<p>theorem</p>		
		</td>
		<td>
			<p>no proof</p>		
		</td>
	</tr>
	<tr>
		<td><p>number of obligations</p></td>
		<td>
			<p><?php echo $anz; echo "(".$sanz.")";
			if ($anz != $sanz) $fehler++;
			?></p>
		</td>
		<td>
			<p><?php echo $yanz; echo "(".$syanz.")";
			if ($yanz != $syanz) $fehler++;
			?></p>
		</td>
		<td>
			<p><?php echo $nanz; echo "(".$snanz.")";
			if ($nanz != $snanz) $fehler++;
			?></p>

		</td>
	</tr>
	<tr>
		<td><p>number of all premises in all obligations</p></td>
		<td>
			<p><?php echo $pra; echo "(".$spra.")";
			if ($pra != $spra) $fehler++;
			?></p>

		</td>
		<td>
			<p><?php echo $ypra; echo "(".$sypra.")";
			if ($ypra != $sypra) $fehler++;
			?></p>

		</td>
		<td>
			<p><?php echo $npra; echo "(".$snpra.")";
			if ($npra != $snpra) $fehler++;
			?></p>

		</td>
	</tr>
	<tr>
		<td><p>average number of premises per obligation</p></td>
		<td>
			<p><?php
				if ($anz > 0) echo round($pra/$anz,2);
				else echo "N/A";
				echo "(".$sppo.")";
			if (round($pra/$anz,2) != $sppo) $fehler++;
			?></p>
		</td>
		<td>
			<p><?php 
				if ($yanz > 0) echo round($ypra/$yanz,2);
				else echo "N/A";
				echo "(".$syppo.")";
			if (round($ypra/$yanz,2) != $syppo) $fehler++;
			?></p>
		</td>
		<td>
			<p><?php
				if ($nanz > 0) echo round($npra/$nanz,2);
				else echo "N/A";
				echo "(".$snppo.")";
			if (round($npra/$nanz,2) != $snppo) $fehler++;
			?></p>
		</td>
	</tr>
	<tr>
		<td><p>average number of used premises per obligation</p></td>
		<td>
			<p><?php
				if ($anz > 0) echo round($banz/$anz,2);
				else echo "N/A";
				echo "(".$suppo.")";
			if (round($banz/$anz,2) != $suppo) $fehler++;
			?></p>
		</td>
		<td>
			<p><?php 
				if ($yanz > 0) echo round($ybanz/$yanz,2);
				else echo "N/A";
				echo "(".$syuppo.")";
			if (round($ybanz/$yanz,2) != $syuppo) $fehler++;
			?></p>
		</td>
		<td>
			<p><?php
				if ($nanz > 0) echo round($nbanz/$nanz,2);
				else echo "N/A";
				echo "(".$snuppo.")";
			if (round($nbanz/$nanz,2) != $snuppo) $fehler++;
			?></p>
		</td>
	</tr>
	<tr>
		<td><p>average number of non-used premises per obligation</p></td>
		<td>
			<p><?php
				if ($anz > 0) echo round($uanz/$anz,2);
				else echo "N/A";
				echo "(".$snnppo.")";
			if (round($uanz/$anz,2) != $snnppo) $fehler++;
			?></p>
		</td>
		<td>
			<p><?php 
				if ($yanz > 0) echo round($yuanz/$yanz,2);
				else echo "N/A";
				echo "(".$synnppo.")";
			if (round($yuanz/$yanz,2) != $synnppo) $fehler++;
			?></p>
		</td>
		<td>
			<p><?php
				if ($nanz > 0) echo round($nuanz/$nanz,2);
				else echo "N/A";
				echo "(".$snnnppo.")";
			if (round($nuanz/$nanz,2) != $snnnppo) $fehler++;
			?></p>
		</td>
	</tr>	
	<tr>
		<td><p>average distance of premises</p></td>
		<td>
			<p><?php
				if ($anzdist > 0) echo round($sum/$anzdist,2);
				else echo "N/A";
				echo "(".$sadp.")";
			if (round($sum/$anzdist,2) != $sadp) $fehler++;
			?></p>
		</td>
		<td>
			<p><?php
				if ($yanzdist > 0) echo round($ysum/$yanzdist,2);
				else echo "N/A";
				echo "(".$syadp.")";
			if (round($ysum/$yanzdist,2) != $syadp) $fehler++;
			?></p>
		</td>
		<td>
			<p><?php
				if ($nanzdist > 0) echo round($nsum/$nanzdist,2);
				else echo "N/A";
				echo "(".$snadp.")";
			if (round($nsum/$nanzdist,2) != $snadp) $fehler++;
			?></p>	
		</td>
	</tr>
	<tr>
		<td><p>average distance of used premises</p></td>
		<td>
			<p><?php
				if ($banz > 0) echo round($bsum/$banz,2);
				else echo "N/A";
				echo "(".$sadup.")";
			if (round($bsum/$banz,2) != $sadup) $fehler++;
			?></p>
		</td>
		<td>
			<p><?php
				if ($ybanz > 0) echo round($ybsum/$ybanz,2);
				else echo "N/A";
				echo "(".$syadup.")";
			if (round($ybsum/$ybanz,2) != $syadup) $fehler++;
			?></p>
		</td>
		<td>
			<p><?php
				if ($nbanz > 0) echo round($nbsum/$nbanz,2);
				else echo "N/A";
				echo "(".$snadup.")";
			if (round($nbsum/$nbanz,2) != $snadup) $fehler++;
			?></p>
		</td>
	</tr>
	<tr>
		<td><p>average distance of non-used premises</p></td>
		<td>
			<p><?php
				if ($uanz > 0) echo round($usum/$uanz,2);
				else echo "N/A";
				echo "(".$sadnp.")";
			if (round($usum/$uanz,2) != $sadnp) $fehler++;
			?></p>
		</td>
		<td>
			<p><?php
				if ($yuanz > 0) echo round($yusum/$yuanz,2);
				else echo "N/A";
				echo "(".$syadnp.")";
			if (round($yusum/$yuanz,2) != $syadnp) $fehler++;
			?></p>
		</td>
		<td>
			<p><?php
				if ($nuanz > 0) echo round($nusum/$nuanz,2);
				else echo "N/A";
				echo "(".$snadnp.")";
			if (round($nusum/$nuanz,2) != $snadnp) $fehler++;
			?></p>
		</td>
	</tr>

	<tr>
		<td><p>minimal distance of obligations</p></td>
		<td>
			<p><?php echo $min; echo "(".$smind.")";
			if ($min != $smind) $fehler++;
			?></p>

		</td>
		<td>
			<p><?php echo $ymin; echo "(".$symind.")";
			if ($ymin != $symind) $fehler++;
			?></p>

		</td>
		<td>
			<p><?php echo $nmin; echo "(".$snmind.")";
			if ($nmin != $snmind) $fehler++;
			?></p>

		</td>
	</tr>
	<tr>
		<td><p>minimal distance of used obligations</p></td>
		<td>
			<p><?php echo $bmin; echo "(".$sminud.")";
			if ($bmin != $sminud) $fehler++;
			?></p>

		</td>
		<td>
			<p><?php echo $ybmin; echo "(".$syminud.")";
			if ($ybmin != $syminud) $fehler++;
			?></p>

		</td>
		<td>
			<p><?php echo $nbmin; echo "(".$snminud.")";
			if ($nbmin != $snminud) $fehler++;
			?></p>

		</td>
	</tr>
	<tr>
		<td><p>minimal distance of non-used obligations</p></td>
		<td>
			<p><?php echo $umin; echo "(".$sminnd.")";
			if ($umin != $sminnd) $fehler++;
			?></p>

		</td>
		<td>
			<p><?php echo $yumin; echo "(".$syminnd.")";
			if ($yumin != $syminnd) $fehler++;
			?></p>

		</td>
		<td>
			<p><?php echo $numin; echo "(".$snminnd.")";
			if ($numin != $snminnd) $fehler++;
			?></p>

		</td>
	</tr>
	<tr>
		<td><p>maximal distance of obligations</p></td>
		<td>
			<p><?php echo $max; echo "(".$smaxd.")";
			if ($max != $smaxd) $fehler++;
			?></p>

		</td>
		<td>
			<p><?php echo $ymax; echo "(".$symaxd.")";
			if ($ymax != $symaxd) $fehler++;
			?></p>

		</td>
		<td>
			<p><?php echo $nmax; echo "(".$snmaxd.")";
			if ($nmax != $snmaxd) $fehler++;
			?></p>

		</td>
	</tr>
	<tr>
		<td><p>maximal distance of used obligations</p></td>
		<td>
			<p><?php echo $bmax; echo "(".$smaxud.")";
			if ($bmax != $smaxud) $fehler++;
			?></p>

		</td>
		<td>
			<p><?php echo $ybmax; echo "(".$symaxud.")";
			if ($ybmax != $symaxud) $fehler++;
			?></p>

		</td>
		<td>
			<p><?php echo $nbmax; echo "(".$snmaxud.")";
			if ($nbmax != $snmaxud) $fehler++;
			?></p>

		</td>
	</tr>
	<tr>
		<td><p>maximal distance of non-used obligations</p></td>
		<td>
			<p><?php echo $umax; echo "(".$smaxnd.")";
			if ($umax != $smaxnd) $fehler++;
			?></p>

		</td>
		<td>
			<p><?php echo $yumax; echo "(".$symaxnd.")";
			if ($yumax != $symaxnd) $fehler++;
			?></p>

		</td>
		<td>
			<p><?php echo $numax; echo "(".$snmaxnd.")";
			if ($numax != $snmaxnd) $fehler++;
			?></p>

		</td>
	</tr>
</table>
<br/>
<? echo "Number of Errors: ".$fehler; ?>
<br/>
<?
}
?>
<?PHP

// showObli: berechnet statistiken zu einer ausgewählten Obligation und zeigt diese anschliessend an
// stats: array mit statistiken zur jeweiligen Obligation
// Rückgabewert:NA
function showObligationStats($stats){

	?>
	<div id="content">
	<?
	$ar = $stats;
	$error = 0;
	echo "<br/><h2>Premises of Obligation:</h2>";
	//echo '<div id="timeSpent"><br/>';
?>

<table border="0" width="88%">
	<tr>
		<td width="80%"><h3>Premises</h3></td>
		<td width="10%"><h3>Used?</h3></td>
		<td width="10%"><h3>Distance</h3></td>
	</tr>
<?PHP
	// Anzeigen der Inhalte:
	for($i = 0; $i < sizeof($ar[2]); $i++) {
		if ($ar[2][$i][1] == "no") {
?>
	<tr>
		<td><font color='black'><? echo $ar[2][$i][0];?></font></td>
		<td><font color='red'><? echo $ar[2][$i][1];?></font></td>
		<td><font color='black'><? echo $ar[2][$i][2];?></font></td>
	</tr>
<?
		}
		else {
?>
	<tr>
		<td><font color='black'><? echo $ar[2][$i][0];?></font></td>
		<td><font color='green'><? echo $ar[2][$i][1];?></font></td>
		<td><font color='black'><? echo $ar[2][$i][2];?></font></td>
	</tr>
<?
		}
	}
	if ($ar[1] == "noproof") {
?>
	<tr>
		<td><font color='black'><b>Conjecture:</b></font></td>
		<td><font color='red'></font></td>
	</tr>
	<tr>
		<td><font color='black'><? echo $ar[0];?></font></td>
		<td><font color='red'><? echo $ar[1];?></font></td>
	</tr>

</table>
<?
	}
	else {
?>
	<tr>
		<td><font color='black'><b>Conjecture:</b></font></td>
		<td><font color='green'></font></td>
	</tr>
	<tr>
		<td><font color='black'><? echo $ar[0];?></font></td>
		<td><font color='green'><? echo $ar[1];?></font></td>
	</tr>
</table>
<?
	}
echo "<br/><br/>";
	// Variablen für Statistiken:
	$sum = 0;
	$ysum = 0;
	$nsum = 0;
	$anz = 0;
	$yanz = 0;
	$nanz = 0;
	$dist = 0;
	$ydist = 0;
	$ndist = 0;
	$ymin = 999999;
	$nmin = 999999;
	$ybmin = 999999;
	$nbmin = 999999;
	$yumin = 999999;
	$numin = 999999;
	$ybmax = 0;
	$nbmax = 0;
	$ymax = 0;
	$nmax = 0;
	$ypra = 0;
	$npra = 0;
	$pra = 0;
	$yanzdist = 0;
	$nanzdist = 0;
	$ybanz = 0;
	$nbanz = 0;
	$ybsum = 0;
	$nbsum = 0;
	$yuanz = 0;
	$nuanz = 0;
	$yusum = 0;
	$nusum = 0;
/*
	$medianTheoremY = 0;
	$medianTheoremN = 0;
	$medianNPY = 0;
	$medianNPN = 0;
//*/
	echo "<br/><br/><h2>Statistical obligations analysis:</h2>";
	// überprüfung theorem/no-proof:
	if($ar[1] == "theorem"){
		//echo "Theo<br/>";
		$yanz++;
		$ypra = $ypra + sizeof($ar[2]);
		for($i = 0; $i < sizeof($ar[2]); $i++) {
			$ydist = $ar[2][$i][2];
			$ysum = $ysum + $ydist;
			$medianTheorem[$i] = $ydist;
			$yanzdist++;
			if ($ymin > $ydist){
				$ymin = $ydist;
				//echo "min<br/>";
			}
			if ($ymax < $ydist){
				$ymax = $ydist;
				//echo "max<br/>";
			}
			// Anzahl benutzer Prämissen
			if($ar[2][$i][1]=="yes"){
				$ybdist = $ar[2][$i][2];
				$medianTheoremY[$ybanz] = $ar[2][$i][2];
				//echo "yes-distanz prä[".$j."]: ".$ydist."<br/>";
				$ybsum = $ybsum + $ybdist;
				$ybanz++;
				if ($ybmin > $ybdist){
					$ybmin = $ybdist;
					//echo "min<br/>";
				}
				if ($ybmax < $ybdist){
					$ybmax = $ybdist;
					//echo "max<br/>";
				}
			}
			else if($ar[2][$i][1]=="no"){
				$yudist = $ar[2][$i][2];
				$medianTheoremN[$yuanz] = $ar[2][$i][2];
				//echo "no-distanz prä[".$j."]: ".$ydist."<br/>";
				$yusum = $yusum + $yudist;
				$yuanz++;
				if ($yumin > $yudist){
					$yumin = $yudist;
					//echo "min<br/>";
				}
				if ($yumax < $yudist){
					$yumax = $yudist;
					//echo "max<br/>";
				}
			}
		}
	}
	else if($ar[1] == "noproof"){
		//echo "Neo<br/>";
		$nanz++;
		$npra = $npra + sizeof($ar[2]);
		for($i = 0; $i < sizeof($ar[2]); $i++) {
			$ndist = $ar[2][$i][2];
			$nsum = $nsum + $ndist;
			$medianNP[$i] = $ndist;
			$nanzdist++;
			if ($nmin > $ndist){
				$nmin = $ndist;
				//echo "min<br/>";
			}
			if ($nmax < $ndist){
				$nmax = $ndist;
				//echo "max<br/>";
			}
			if($ar[2][$i][1]=="yes"){
				$nbdist = $ar[2][$i][2];
				$medianNPY[$nbanz] = $ar[2][$i][2];
				//echo "yes-distanz prä[".$j."]: ".$ydist."<br/>";
				$nbsum = $nbsum + $nbdist;
				$nbanz++;
				if ($nbmin > $nbdist){
					$nbmin = $nbdist;
					//echo "min<br/>";
				}
				if ($nbmax < $nbdist){
					$nbmax = $nbdist;
					//echo "max<br/>";
				}
			}
			else if($ar[2][$i][1]=="no"){
				$nudist = $ar[2][$i][2];
				$medianNPN[$nuanz] = $ar[2][$i][2];
				//echo "no-distanz prä[".$j."]: ".$ydist."<br/>";
				$nusum = $nusum + $nudist;
				$nuanz++;
				if ($numin > $nudist){
					$numin = $nudist;
					//echo "min<br/>";
				}
				if ($numax < $nudist){
					$numax = $nudist;
					//echo "max<br/>";
				}
			}
		}
	}
	$anz = $yanz + $nanz;
	$pra = $ypra + $npra;
	$anzdist = $yanzdist + $nanzdist;
	$sum = $ysum + $nsum;
	if ($nmin > $ymin){$min = $ymin;}
	else {$min = $nmin;}
	
	if ($nbmin > $ybmin){$bmin = $ybmin;}
	else {$bmin = $nbmin;}
	
	if ($numin > $yumin){$umin = $yumin;}
	else {$umin = $numin;}
	
	if ($nbmax < $ybmax){$bmax = $ybmax;}
	else {$bmax = $nbmax;}
	
	if ($numax < $yumax){$umax = $yumax;}
	else {$umax = $numax;}
	
	if ($nmax < $ymax){$max = $ymax;}
	else {$max = $nmax;}
	
	if ($min == 999999) $min = "N/A";
	if ($ymin == 999999) $ymin = "N/A";
	if ($nmin == 999999) $nmin = "N/A";
	if ($bmin == 999999) $bmin = "N/A";
	if ($umin == 999999) $umin = "N/A";
	if ($yumin == 999999) $yumin = "N/A";
	if ($numin == 999999) $numin = "N/A";
	if ($ybmin == 999999) $ybmin = "N/A";
	if ($nbmin == 999999) $nbmin = "N/A";
	
	if ($max == 0) $max = "N/A";
	if ($ymax == 0) $ymax = "N/A";
	if ($nmax == 0) $nmax = "N/A";
	if ($bmax == 0) $bmax = "N/A";
	if ($umax == 0) $umax = "N/A";
	if ($ybmax == 0) $ybmax = "N/A";
	if ($yumax == 0) $yumax = "N/A";
	if ($nbmax == 0) $nbmax = "N/A";
	if ($numax == 0) $numax = "N/A";
	
	$bsum = $ybsum + $nbsum;
	$banz = $ybanz + $nbanz;
	$usum = $yusum + $nusum;
	$uanz = $yuanz + $nuanz;

	// Ausgabe der berechneten Werte in einer Tabelle
	?>
	<br/>
	<table border="0" width="88%" cellspacing="0" cellpadding="0">
		<tr>
			<td width="80%"><h3>Obligation stats</h3></td>
			<?PHP 
			if ($yanz > 0){ 
				?>
				<td>
					<p>theorem</p>
				</td>
				<?PHP 
			}
			else if ($nanz > 0){ 
				?>
				<td>
					<p>no proof</p>
				</td>
			<?PHP } ?>
		</tr>
	
		<tr>
			<td>average number of premises</td>
	<?PHP 
	if ($yanz > 0){ 
	?>
			<td>
				<?php 
					if ($yanz > 0) echo round($ypra/$yanz,2);
					else echo "N/A";
				?>
			</td>
	<?PHP 
	}
	else if ($nanz > 0){ 
	?>
			<td>
				<?php
					if ($nanz > 0) echo round($npra/$nanz,2);
					else echo "N/A";
				?>
			</td>
	<?PHP } ?>
		</tr>
		<tr>
			<td>average number of used premises</td>
	<?PHP 
	if ($yanz > 0){ 
	?>
			<td>
				<?php 
					if ($yanz > 0) echo round($ybanz/$yanz,2);
					else echo "N/A";
				?>
			</td>
	<?PHP 
	}
	else if ($nanz > 0){ 
	?>
			<td>
				<?php
					if ($nanz > 0) echo round($nbanz/$nanz,2);
					else echo "N/A";
				?>
			</td>
	<?PHP } ?>
		</tr>
		<tr>
			<td>average number of non-used premises</td>
	<?PHP 
	if ($yanz > 0){ 
	?>
			<td>
				<?php 
					if ($yanz > 0) echo round($yuanz/$yanz,2);
					else echo "N/A";
				?>
			</td>
	<?PHP 
	}
	else if ($nanz > 0){ 
	?>
			<td>
				<?php
					if ($nanz > 0) echo round($nuanz/$nanz,2);
					else echo "N/A";
				?>
			</td>
	<?PHP } ?>
		</tr>	
		<tr>
			<td><hr/></td><td><hr/></td>
		</tr>	
		<tr>
			<td>average distance of premises</td>
	<?PHP 
	if ($yanz > 0){ 
	?>
			<td>
				<?php
					if ($yanzdist > 0) echo round($ysum/$yanzdist,2);
					else echo "N/A";
				?>	
			</td>
	<?PHP 
	}
	else if ($nanz > 0){ 
	?>
			<td>
				<?php
					if ($nanzdist > 0) echo round($nsum/$nanzdist,2);
					else echo "N/A";
				?>	
			</td>
	<?PHP } ?>
		</tr>
		<tr>
			<td>average distance of used premises</td>
	<?PHP 
	if ($yanz > 0){ 
	?>
			<td>
				<?php
					if ($ybanz > 0) echo round($ybsum/$ybanz,2);
					else echo "N/A";
				?>
			</td>
	<?PHP 
	}
	else if ($nanz > 0){ 
	?>
			<td>
				<?php
					if ($nbanz > 0) echo round($nbsum/$nbanz,2);
					else echo "N/A";
				?>
			</td>
	<?PHP } ?>
		</tr>
		<tr>
			<td>average distance of non-used premises</td>
	
	<?PHP 
	if ($yanz > 0){ 
	?>
			<td>
				<?php
					if ($yuanz > 0) echo round($yusum/$yuanz,2);
					else echo "N/A";
				?>
			</td>
	<?PHP 
	}
	else if ($nanz > 0){ 
	?>
			<td>
				<?php
					if ($nuanz > 0) echo round($nusum/$nuanz,2);
					else echo "N/A";
				?>
			</td>
	<?PHP } ?>
		</tr>
		<tr>
			<td><hr/></td><td><hr/></td>
		</tr>	
		<tr>
			<td>minimal distance of obligation</td>
	
	<?PHP 
	if ($yanz > 0){ 
	?>
			<td>
				<?php echo $ymin;?>
			</td>
	<?PHP 
	}
	else if ($nanz > 0){ 
	?>
			<td>
				<?php echo $nmin;?>
			</td>
	<?PHP } ?>
		</tr>
		<tr>
			<td>minimal distance of used obligation</td>
	<?PHP 
	if ($yanz > 0){ 
	?>
			<td>
				<?php echo $ybmin;?>
			</td>
	<?PHP 
	}
	else if ($nanz > 0){ 
	?>
			<td>
				<?php echo $nbmin;?>
			</td>
	<?PHP } ?>
		</tr>
		<tr>
			<td>minimal distance of non-used obligation</td>
	<?PHP 
	if ($yanz > 0){ 
	?>
			<td>
				<?php echo $yumin;?>
			</td>
	<?PHP 
	}
	else if ($nanz > 0){ 
	?>
			<td>
				<?php echo $numin;?>
			</td>
	<?PHP } ?>
		</tr>
		<tr>
			<td><hr/></td><td><hr/></td>
		</tr>	
		<tr>
			<td>maximal distance of obligation</td>
	<?PHP 
	if ($yanz > 0){ 
	?>
			<td>
				<?php echo $ymax;?>
			</td>
	<?PHP 
	}
	else if ($nanz > 0){ 
	?>
			<td>
				<?php echo $nmax;?>
			</td>
	<?PHP } ?>
		</tr>
		<tr>
			<td>maximal distance of used obligation</td>
	<?PHP 
	if ($yanz > 0){ 
	?>
			<td>
				<?php echo $ybmax;?>
			</td>
	<?PHP 
	}
	else if ($nanz > 0){ 
	?>
			<td>
				<?php echo $nbmax;?>
			</td>
	<?PHP } ?>
		</tr>
		<tr>
			<td>maximal distance of non-used obligation</td>
	<?PHP 
	if ($yanz > 0){ 
	?>
			<td>
				<?php echo $yumax;?>
			</td>
	<?PHP 
	}
	else if ($nanz > 0){ 
	?>
			<td>
				<?php echo $numax;?>
			</td>
	<?PHP } ?>
		</tr>
			<td><hr/></td><td><hr/></td>
		<tr>
			<td>median distance of obligation</td>
	<?PHP 
	if ($yanz > 0){ 
	?>
			<td>
				<?php echo getMedian($medianTheorem);?>
			</td>
	<?PHP 
	}
	else if ($nanz > 0){ 
	?>
			<td>
				<?php echo getMedian($medianNP);?>
			</td>
	<?PHP } ?>
		</tr>
		<tr>
			<td>median distance of used obligation</td>
	<?PHP 
	if ($yanz > 0){ 
	?>
			<td>
				<?php echo getMedian($medianTheoremY);?>
			</td>
	<?PHP 
	}
	else if ($nanz > 0){ 
	?>
			<td>
				<?php echo getMedian($medianNPY);?>
			</td>
	<?PHP } ?>
		</tr>
		<tr>
			<td>median distance of non-used obligation</td>
	<?PHP 
	if ($yanz > 0){ 
	?>
			<td>
				<?php echo getMedian($medianTheoremN);?>
			</td>
	<?PHP 
	}
	else if ($nanz > 0){ 
	?>
			<td>
				<?php echo getMedian($medianNPN);?>
			</td>
	<?PHP } ?>
		</tr>
	</table>
	<br/>
	<?
}

function getMedian($median){
	$anz = sizeof($median);
	$pos = ($anz / 2) -1;// -1 da element 1 an pos 0!
	//echo "Pos: ".$pos."!<br/>";
	if ($anz == 0){
		$erg = "N/A";
	}
	else if ($anz%2 == 0){
		sort($median);
		$erg = $median[$pos] + $median[$pos+1];
		$erg = $erg / 2;
	}
	else {	
		sort($median);
		$erg = $median[$pos+1];
	}
	
	return $erg;
}

?>