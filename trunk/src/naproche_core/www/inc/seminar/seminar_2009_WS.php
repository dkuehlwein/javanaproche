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

<p>Vortr&auml;ge von internen und externen Referenten &uuml;ber Formale  Mathematik, d.h., &uuml;ber die axiomatische Durchf&uuml;hrung von Mathematik in  strikt formalen Sprachen mit strikt formalen Ableitungsregeln. Im  Zusammenhang mit dem lokalen Projekt Naproche (Natural language Proof  Checking) wird besonders die Frage diskutiert, inwieweit durch den  Einsatz von Softwaresystemen strikt formale Systeme aufgebaut werden  k&ouml;nnen, die f&uuml;r den Benutzer &quot;nat&uuml;rlich&quot; im Sinne des gew&ouml;hnlichen  mathematischen Arbeitens sind. Das bezieht sich auf  Benutzerschnittstellen, verwendete Sprache, Theorien und Beweismethoden.</p>
<p>Die Vortr&auml;ge finden etwa 14-t&auml;gig statt.&nbsp;</p>
<br/>

<dl>
	<dt> <strong> 30 November, 16:30-18:00 at Endenicher Allee 60, room 1.007. </strong> <a href="http://www.andrew.cmu.edu/user/avigad/"> Jeremy Avigad </a> (Carnegie Mellon University and INRIA-Microsoft Research Joint Centre, Orsay) <em> A formal system for Euclidean diagrammatic reasoning </em> </dt> <br>

	<dd> This talk presents work carried out jointly with Ed Dean and John Mumma. <br><br>
        For more than two thousand years, Euclid's <em>Elements</em> was viewed as the paradigm of rigorous argumentation. But this changed in the nineteenth century, with concerns over the use of diagrammatic inferences and their ability to secure general validity. Axiomatizations by Pasch, Hilbert, and later Tarski are now taken to rectify these shortcomings, but proofs in these axiomatic systems look very different from Euclid's. <br><br>
        In this talk, I will argue that proofs in the <em>Elements</em>, taken at face value, can be understood in formal terms. I will describe a formal system with both diagram- and text-based inferences that provides a much more faithful representation of Euclidean reasoning. For the class of theorems that can be expressed in the language, the system is sound and complete with respect to Euclidean fields, that is, the semantics corresponding to ruler and compass constructions. <br><br>
        The system's one-step inferences are smoothly verified by current automated reasoning technology. This makes it possible to formally verify Euclidean diagrammatic proofs, and provides useful insight into the nature of mathematical proof more generally. </dd>

</dl>

<dl>
	<dt> <strong> 18 December, 15.15-16.45  at Endenicher Allee 60, room 006</strong> <a href="http://www.math.chalmers.se/~aarne/"> Aarne Ranta </a> (University of Gothenburg) <em> GF and the Language of Mathematics </em> </dt> <br>

	<dd>GF (<a href="http://digitalgrammars.com/gf">Grammatical Framework</a>) is a grammar
formalism designed for multilingual grammars. A multilingual grammar
is a system based on a shared semantic structure with reversible
mappings to several languages. By means of these mappings, it is
possible to translate between all the included languages. For instance,
the semantic structure (Gt x y) might have the translations "x is greater
than y" (English), "x ist gr&ouml;&szlig;er als y" (German), "x &#62; y" (mathematical
symbolism). The format for defining semantic structures is a higher-order
dependently typed lambda calculus, which is expressive enough for
formalizing usual mathematical theories and logics. The format for
defining translations is expressive enough for dealing with grammatical
structures such as German word order, so that e.g. "x &#62; y -&#62; ~ y &#60; x"
gets correctly translated "wenn x gr&ouml;&szlig;er als y ist, dann ist y nicht
gr&ouml;&szlig;er als x".<br><br>

	GF has been used in many implementations of technical translation systems
and natural language interfaces. One example is the European project
<a href="http://webalt.math.helsinki.fi/content/index_eng.html">WebALT</a>, where
mathematical exercises are automatically translated from OpenMATH
specifications to seven European languages. The project developed
a library of GF translation rules for hundreds of mathematical concepts,
which is available as open-source software.<br><br>

	The talk will give a hands-on introduction to GF, working through an
example grammar for mathematical language. We will also discuss the
problems and limitations of the approach, in the light of previous
experiences from projects on mathematical language.</dd>
	
</dl>

<dl>
	<dt> <strong> 29 January, 9:00 - 17:00 at Endenicher Allee 60, room 208 </strong>  <em> Naproche Progress Meeting </em> </dt> <br>
	<dd> 
	<I>
	<LI> 9:15 - 10:00 Sebastian Zittermann: The Naproche WebInterface </LI>
	<LI> 10:00 - 11:00 Daniel K&uuml;hlwein The Premise Selection Algorithm </LI>
	<LI> 11:15 - 12:00  Marcos Cramer: Neue linguistische Konstruktionen in Naproche</LI>
	<LI> 12:00 - 13:30  Mittagspause</LI>
	<LI> 13:30 - 14:15  Diskussion zur Formelgrammatik</LI>
	<LI> 14:30 - 15:30  Diskussion zum Gebrauch von GF in Naproche</LI>
	<LI> 15:45 - 16:45  Diskussion zu Euklid in Naproche</LI>
	</ul>	
	</dd>
	
</dl>

<dl>
<dt> <strong> 5 February 10:00-12:00 in room 006 at EA 60</strong> <a href="http://www.mat.univie.ac.at/~schodl/" > Peter Schodl </a> Universität Wien  </dt> <br>

	<dd>

"MoSMath -- A MOdeling System for MATHematics"

by Peter Schodl, University of Vienna (Austria)
<br> <br>
The goal of the MoSMath project, carried out at the University of Vienna under
the supervision of Prof. Neumaier, is the creation of a software package that is
able to understand, represent and interface optimization problems posed in a
controlled natural language. <br>

We developped a user-friendly representation of semantic information in a data
structure called the semantic matrix. This representation is designed to be
human intelligible and clear, akin to the Semantic Web. A type system was
created that is suited for the typing of both usual data structures and
grammatical categories in the semantic matrix. We also developped a semantic
Turing machine (STM), a variant of a register machine that combines the
transparency and simplicity of the action of a Turing machine with a clearly
arranged assembler-style programming language and a memory in the form of a
semantic matrix. <br>

As a first step towards our controlled natural language we derived a
context-free grammar from a textbook on calculus and linear algebra. We
currently have an interface to and a representation of problems in the TPTP, and
a representation of a number of optimization problems from the OR-Library. <br>

An interface to the controlled natural language of Naproche (developed in Bonn
and Duisburg-Essen for representing human-readable formal proofs) enables us to read and
represent texts written in this language, and to recreate Naproche-texts from
texts represented in the semantic matrix.
	</dd>
</dl>


<dl>
	<dt> <strong> 5 February 14:00-16:00 in room 1007 at EA 60</strong> <a href="http://www.mat.univie.ac.at/~neum/" > Arnold Neumaier</a> Universität Wien </dt> <br>

	<dd>
	
     Towards a Computer-Aided System for Real Mathematics

                     by Arnold Neumaier

              University of Vienna (Austria)

<br> <br>
This is joint work with Peter Schodl and Kevin Kofler, also from Vienna.
We are currently working towards the creation of an automatic
mathematical research system that can support mathematicians in their
daily work, providing services for abstract mathematics as easily as
Latex provides typesetting service, the arXiv provides access to
preprints, Google provides web services, Matlab provides numerical
services, or Mathematica provides symbolic services. <br>

This is partly a vision, expected to take 50 man years to bring a
system far enough that it will grow by itself in a wikipedia-like
fashion. A limited part of the goals are being realized through the
project ``A modeling system for mathematics'' (MoSMath), currently
supported by a grant of the Austrian Science Foundation FWF. <br>

Within this project, we attempt to create a modeling and documentation
language for conceptual and numerical mathematics called FMathL
(formal mathematical language), suited to the habits of mathematicians. <br>

The goal of the FMathL project is to combine <br>
-- the advantages of LaTeX for writing and viewing mathematics, <br>
-- the user-friendliness of mathematical modeling systems such as AMPL
 for the flexible definition of large-scale numerical analysis
 problems, <br>
-- the universality of the common mathematical language to describe
 completely arbitrary problems, <br>
-- the high-level discipline of the CVX system for solving
 convex programming problems and enforcing their semantic correctness,
 and <br>
-- the semantic clarity of the Z notation for the precise
 specification of concepts and statements. <br> <br>

We believe that this goal is reachable, and that an easy-to-use
such system will change the way mathematical modeling is done in
practice. <br>

The project complements efforts for formalizing mathematics from
the computer science and automated theorem proving perspective.
In the long run, the FMathL system might turn into a user-friendly
automatic mathematical assistant for retrieving, editing, and checking
mathematics in both informal, partially formalized, and completely
formalized mathematical form.
	</dd>

</dl>

<h3>Former Terms</h3>
<ul>
<li> <a href="seminar_2007_SS.php">SS 07</a> </li>
<li> <a href="seminar_2008_WS.php">WS 08/09</a> </li>
<li> <a href="seminar_2009_SS.php">SS 09</a> </li>
</ul>

<p><br/></p>

<h3>&nbsp;</h3>
<div id="siteInfo"><a href="#"></a>Last changed November 26st 2009 </div>

</div>
</body>
</html> 
