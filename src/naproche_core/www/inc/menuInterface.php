<div id="navBar">
	<div class="Institut">
		<a href="http://www.math.uni-bonn.de/people/logic/">
			<img src="..<?echo $pfad;?>/img/bonn-logic-header-mini.jpg" alt="Bonn Mathematical Logic Group" width="142" height="52" longdesc="..<?echo $pfad;?>/img/bonn-logic-header-mini.jpg"/>
		</a>
	</div>
	<div class="relatedLinks">
		<h3>Menu</h3>
		<p><a href="../index.php">home</a></p>
		<p><a href="..<?echo $pfad;?>/inc/members.php">members</a></p>
		<hr/>
		<?
			if($debugMode == "on"){
		?>
		<h2><a href="..<?echo $pfad;?>/inc/webinterface.php?DBM=on">web interface</a></h2>
		<?
			}
			else {
		?>
		<h2><a href="..<?echo $pfad;?>/inc/webinterface.php">web interface</a></h2>
		<?
			}
		?>

		<h5><br/></h5>
		<div id="examples">
		<?
			if($debugMode == "on"){
		?>
			<h3>examples</h3><br/>
			<a href="..<?echo $pfad;?>/inc/webinterface.php?proof=1&DBM=on">Burali-Forti paradox</a><br/>
			<a href="..<?echo $pfad;?>/inc/webinterface.php?proof=2&DBM=on">Group Theory</a><br/>
			<a href="..<?echo $pfad;?>/inc/webinterface.php?proof=3&DBM=on">Landau</a><br/>
		<?
			}
			else {
		?>
			<h3>examples</h3><br/>
			<a href="..<?echo $pfad;?>/inc/webinterface.php?proof=1">Burali-Forti paradox</a><br/>
			<a href="..<?echo $pfad;?>/inc/webinterface.php?proof=2">Group Theory</a><br/>
			<a href="..<?echo $pfad;?>/inc/webinterface.php?proof=3">Landau</a><br/>
		<?
			}
		?>
		</div>
        	<p><a href="http://www.naproche.net/wiki/doku.php?id=dokumentation:language">tutorial</a></p>
		<hr/>
		<p><a href="..<?echo $pfad;?>/inc/seminar/seminar_2010_SS.php">formal mathematics seminar </a></p>
		<p><a href="..<?echo $pfad;?>/inc/downloads.php">downloads</a></p>
		<p><a href="<?echo $pfad;?>inc/internship.php">internship</a></p>
	</div>
	<div class="Institut">
		<a href="http://www.math.uni-bonn.de"><img src="..<?echo $pfad;?>/img/Mathematisches_Institut_button_picture.jpg" alt="Mathematisches Institut" width="144" height="56" longdesc="Mathematisches_Institut_button_picture.jpg" /></a>
	</div>
</div>
