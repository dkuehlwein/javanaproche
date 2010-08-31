<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
        "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="de" lang="de">
  <head>
<link rel="stylesheet" href="/naproche/css/Naproche.css" type="text/css" media="screen, projection" />
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
    <title>Naproche WebInterface</title>
    <meta name="description" content="Naproche Webinterface." />
    <meta name="language" content="de" />
    <meta name="robots" content="index, follow" />
  </head>
<body>

<?php
error_reporting(0);
ini_set('display_errors', 1);
$datei = "array3.php";

$test = "t".$datei;
$erg = "erg".$datei;
// Menu
include($_SERVER["DOCUMENT_ROOT"].'/naproche/inc/header.php');
include($_SERVER["DOCUMENT_ROOT"].'/naproche/inc/menu.php');

//include($_SERVER["DOCUMENT_ROOT"].'naproche/inc/'.$erg);
//include($_SERVER["DOCUMENT_ROOT"].'naproche/inc/'.$test);

//include($_SERVER["DOCUMENT_ROOT"].'inc/stats.php');

include($_SERVER["DOCUMENT_ROOT"].'/naproche/inc/test/obliarr.php');
include($_SERVER["DOCUMENT_ROOT"].'/naproche/inc/stats.php');

//showStats($stats);
echo $obli;
showObligationStats($obli);


//compareStats($stats,($_SERVER["DOCUMENT_ROOT"].'inc/'.$erg));
//showStats($stats);

// Foot Matter
include($_SERVER["DOCUMENT_ROOT"].'/naproche/inc/footer.html');
?>
