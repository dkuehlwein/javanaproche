<?
include('../inc/init.php');
if ($debugMode != "on"){
	$debugMode = "off";
}

$pfad = "../";

include('../inc/htmlhead.php');
include('../inc/header.php');
$pfad = "";
include('../inc/menuInterface.php');
// Main
include('../inc/main.php');
//include('../inc/main.html');
// Foot Matter
include('../inc/footer.html');
?>
