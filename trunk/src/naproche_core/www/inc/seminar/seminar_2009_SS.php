<?PHP
$pfad = "../../";
include('../../inc/htmlhead.php');
include('../../inc/header.php');
include('../../inc/menu.php');
?>

<div id="content">
  <!-- Body Text START EDITING HERE!!! -->
<h2>Seminar Formale Mathematik   </h2>
<h3>Dozenten</h3>

<ul>
<li>Prof. Dr. <a href="http://www.math.uni-bonn.de/people/logic/People/Koepke.html">
Peter Koepke</a></li>
<li>Prof. Dr. <a href="http://www.bernhard-schroeder.eu/">Bernhard Schr&ouml;der</a> </li>
</ul>
<h3>Zeit und Ort </h3>
<p>Freitags  10-12 im Zimmer 006.</p>
<h3>Inhalt</h3>

<p>Vortr&auml;ge von internen und externen Referenten &uuml;ber Formale  Mathematik, d.h., &uuml;ber die axiomatische Durchf&uuml;hrung von Mathematik in  strikt formalen Sprachen mit strikt formalen Ableitungsregeln. Im  Zusammenhang mit dem lokalen Projekt NaProChe (Natural language Proof  Checking) wird besonders die Frage diskutiert, inwieweit durch den  Einsatz von Softwaresystemen strikt formale Systeme aufgebaut werden  k&ouml;nnen, die f&uuml;r den Benutzer &quot;nat&uuml;rlich&quot; im Sinne des gew&ouml;hnlichen  mathematischen Arbeitens sind. Das bezieht sich auf  Benutzerschnittstellen, verwendete Sprache, Theorien und Beweismethoden.</p>
<p>Die Vortr&auml;ge finden etwa 14-t&auml;gig statt.&nbsp;</p>

<br/>
<p> <b>Friday, April 17, 10:15-12:00</b>, EA 60, Room 006
<br/>
<b>Andrei Paskevich</b>, ForTheL: design decisions [not [yet] taken]
<br/>
Abstract: 
<i>Historically, the best-known and most powerful proof assistants were
the systems with quite specific logical foundations, input languages,
and proof development tools. This created a considerable barrier for
newcomers, even for those who were experienced in formal mathematics.
In recent years, however, the idea to lower this barrier -- by using
(elements of) natural language in the input, by adopting traditional
"declarative" style of proof, by abstracting over the particularities
of a given verifier -- gained a lot of attention, and a number of new
projects have emerged.
<br/>
In this talk, we present a formal language, called ForTheL, which 
imitates the natural language and style of "human" mathematical texts.
This language is used as the input language of SAD, a proof assistant 
intended for automated proof verification. We demonstrate that ForTheL 
allows for terse and comprehensible formalizations and describe its 
syntax and semantics with respect to verification. We also enumerate 
the features that the current realization of ForTheL is lacking and
discuss the ways to address these shortcomings.
</i>
</p>

<p> <b>Thursday, Mai 14</b>, EA 60, Room 208 until 16:30 and then Room 107 
<br/>
<b> Workshop VeriMathDoc - Naproche</b>
<br/>

9:00 - 11:00 Session Formalisierung
Formalisierung: Es gibt i.A. mehrere M&ouml;glichkeiten, wie die
beschriebenen Konzepte in Logik formalisiert werden (Beispiel: der
Begriff einer "Klasse" als Sorte/Typ oder als Praedikat). Je
nachdem wie man sich entscheidet, hat das Vor- und Nachteile und es
scheint keine a priori bessere Art zu formalisieren zu geben. Aber,
in dem Prozess nach der Textanalyse hin zum Beweissystem muss die
Entscheidung getroffen werden und die Frage ist wie man sie trifft,
welche Probleme es hier gibt und allgemein, wie kann man damit
umgehen, dass es Alternativen gibt?<br/><br/>
- Vortrag "M&ouml;glichkeiten Klassen zu formalisieren"<br/>
- Diskussion:<br/>
 + Inwieweit legen wir die Art der Formalisierung in den Systemen fest?<br/>
 + Falls wir sie nicht festlegen, wie kann der Autor sie angeben?<br/>
 + Wollen wir mit alternativen Formalisierungen umgehen koennen?<br/>
- Fazit und Planung bis zum n&auml;chsten Treffen<br/><br/>

11:00 - 13:00: Session Corpora of Documents<br/>
Sowohl VeriMathDoc als auch Naproche ben&ouml;tigen Beispieltexte
um die F&auml;higkeiten des jeweiligen Systems zu demonstrieren.
In dieser Session wollten wir er&ouml;rtern inwieweit eine gemeinsame
Sammlung m&ouml;glich ist.<br/>
- Vortrag Syntax Naproche     ( 15 - 30 min)<br/>
- Vortrag Syntax VeriMathDoc ( 15 - 30 min)<br/>
- Diskussion:<br/>
 + Ist eine gemeinsame Syntax m&ouml;glich? Wie m&uuml;sste man die bestehenden
    erweitern/anpassen.<br/>
 + Welche Arten von Dokumente (vollstaendig/Fragmente) wollen wir
    annotieren und in welchen Formaten?  Wie wollen wir darin suchen
    koennen? Wo bekommen wir die her?<br/>
- Fazit und Planung bis zum n&auml;chsten Treffen<br/><br/>

13:00 - 14:00 Mittagessen<br/><br/>

14:00 - 16:00: Session Textanalyse<br/>
Beide Projekte verwenden sehr verschiedene Methoden zur Textanalyse.
Was die Probleme der jeweiligen Methoden, und was k&ouml;nnten L&ouml;sungsans&auml;tze
sein.<br/>
- Vortrag Probleme Textanalyse Naproche     ( ~15 min)<br/>
- Vortrag Probleme Textanalyse VeriMathDoc ( ~15 min)<br/>
- Diskussion: Kann man die die Methoden kombinieren um die Probleme zu l&ouml;sen.<br/>
 + Wie gehen wir mit Fehlschl&auml;gen in der Textanalyse um? Wie geht
    man mit Texten um, die man erkennbar nur partiell analysieren
    kann? Sind partielle Analyseergebnisse (etwa aus einer Chart)
    verwendbar?<br/>
 + Dom&auml;nenspezifische Semantik/Pragmatik: K&ouml;nnen wir Meta-Aussagen
    (zB "Wir geben nun zwei weitere Regeln f&uuml;r das Rechnen mit
    Vektorprodukten an, von denen wir die erste gleich und die zweite
    im n&auml;chsten Abschnitt beweisen werden." [1]) mit flachen
    Verfahren hinreichend erkennen?<br/>
- Fazit und Planung bis zum n&auml;chsten Treffen<br/><br/>

16:00 - 16:30 Kaffee<br/><br/>

16:30 - 18:30 Session: Beweistechniken<br/>
Abgesehen von den angeschlossenen Beweisern ist oft auch eine Vorverarbeitung
w&uuml;nschenswert. Was wurde hier schon implementiert, welche Probleme sind aufgetreten.<br/>
- Vortrag Beweistechniken Naproche     ( 15 - 30 min)<br/>
- Vortrag Beweistechniken VeriMathDoc ( 15 - 30 min)<br/>
- Diskussion:<br/>
 + Potentielle Probleml&ouml;sungen<br/>
- Fazit und Planung bis zum n&auml;chsten Treffen<br/>	

</p>

<p> <b>Friday, May 29, 10:15-12:00</b>, EA 60, Room 006
<br/>
<b>Marcos Cramer</b>: The Naproche Project, Controlled Natural Language Proof Checking of Mathematical Texts
<br/>
Abstract: 
<i>This is the talk that we will give at the CNL (Controlled Natural Language) Conference in Marettimo in June. 
<br/>
We will discuss the Semi-Formal Language of Mathematics, present the Naproche CNL that is based on it, and explain the Proof Representation Structures that can be derived from a Naproche text.
<br/>
- <a href="<?echo $pfad;?>downloads/2009/CNL.pdf">Extended Abstract of this talk</a>
</i>
</p>

<p> <b>Friday, June 19, 10:15-11:00</b>, EA 60, Room 006
<br/>
<b>Daniel Kuehlwein</b>: The Naproche System
<br/>
Abstract: 
<i>This is the talk that we will give at the Calculemus Conference. 
<br/>
We give an introduction to the Naproche System.
<br/>
- <a href="<?echo $pfad;?>downloads/2009/calculemus.pdf">Extended Abstract of this talk</a>
</i>
</p>

<p> <b>Friday, June 19, 11:15-12:00</b>, EA 60, Room 006
<br/>
<b>Sebastian Zittermann</b>: Scheme2XML
<br/>
Abstract: 
<i>
Der Vortrag stellt das Programm Scheme2XML vor.
Scheme2XML erstellt aus einer Datei des Texmacs-Formats "SCM" anhand vorgegebener Regeln, die in der Datei "Satz.dtd" definiert sind, eine XML-Datei und &uuml;berpr&uuml;ft diese auf Validit&auml;t.
<br/>
- <a href="http://opus.bibl.fh-koeln.de/frontdoor.php?source_opus=197">Zugrundeliegende Bachelorarbeit</a>
</i>
</p>

<h3>Former Terms</h3>
<ul>
<li> <a href="seminar_2008_WS.php">WS 08/09</a> </li>
</ul>

<p><br />
</p>

<h3>&nbsp;</h3>
<div id="siteInfo"><a href="#"></a>Last changed October 12st 2009 </div>

</div>
</body>
</html> 
