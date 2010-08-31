:- module(dcg_lexicon,[dcg_lexicon/2,dcg_lexicon/3,dcg_lexicon/4]).


%============================
% Nouns, pronouns and numbers
%============================

% Simple nouns
dcg_lexicon([set],noun,number~singular..noun_type~collection,[set]).
dcg_lexicon([sets],noun,number~plural..noun_type~collection,[set]).
dcg_lexicon([class],noun,number~singular..noun_type~collection,[class]).
dcg_lexicon([classes],noun,number~plural..noun_type~collection,[class]).
dcg_lexicon([collection],noun,number~singular..noun_type~collection,[collection]).
dcg_lexicon([collection],noun,number~plural..noun_type~collection,[collection]).
dcg_lexicon([element],noun,number~singular..noun_type~normal,[element]).
dcg_lexicon([elements],noun,number~plural..noun_type~normal,[element]).
dcg_lexicon([number],noun,number~singular..noun_type~normal,[number]).
dcg_lexicon([numbers],noun,number~plural..noun_type~normal,[number]).
dcg_lexicon([integer],noun,number~singular..noun_type~normal,[integer]).
dcg_lexicon([integers],noun,number~plural..noun_type~normal,[integer]).
dcg_lexicon([real],noun,number~singular..noun_type~normal,[real]).
dcg_lexicon([reals],noun,number~plural..noun_type~normal,[real]).
dcg_lexicon([ordinal],noun,number~singular..noun_type~normal,[ordinal]).
dcg_lexicon([ordinals],noun,number~plural..noun_type~normal,[ordinal]).
dcg_lexicon([point],noun,number~singular..noun_type~normal,[point]).
dcg_lexicon([points],noun,number~plural..noun_type~normal,[point]).
dcg_lexicon([line],noun,number~singular..noun_type~normal,[line]).
dcg_lexicon([lines],noun,number~plural..noun_type~normal,[line]).
dcg_lexicon([circle],noun,number~singular..noun_type~normal,[circle]).
dcg_lexicon([circles],noun,number~plural..noun_type~normal,[circle]).
dcg_lexicon([segment],noun,number~singular..noun_type~normal,[segment]).
dcg_lexicon([segments],noun,number~plural..noun_type~normal,[segments]).
dcg_lexicon([angle],noun,number~singular..noun_type~normal,[angle]).
dcg_lexicon([angles],noun,number~plural..noun_type~normal,[angle]).
dcg_lexicon([area],noun,number~singular..noun_type~normal,[area]).
dcg_lexicon([areas],noun,number~plural..noun_type~normal,[area]).
dcg_lexicon([triangle],noun,number~singular..noun_type~normal,[triangle]).
dcg_lexicon([triangles],noun,number~plural..noun_type~normal,[triangle]).

% Complex nouns
dcg_lexicon([natural,number],noun,number~singular..noun_type~normal,[natural,number]).
dcg_lexicon([natural,numbers],noun,number~plural..noun_type~normal,[natural,number]).

% Pronouns
dcg_lexicon([it],pronoun,number~singular).

% Numbers
dcg_lexicon([two],number).
dcg_lexicon([three],number).
dcg_lexicon([four],number).
dcg_lexicon([five],number).
dcg_lexicon([six],number).
dcg_lexicon([seven],number).
dcg_lexicon([eight],number).
dcg_lexicon([nine],number).
dcg_lexicon([ten],number).


%====================================
% Adjectives, verbs and prepositions
%====================================

dcg_lexicon([empty],adjective,adj_trans~no).
dcg_lexicon([even],adjective,adj_trans~no).
dcg_lexicon([natural],adjective,adj_trans~no).
dcg_lexicon([odd],adjective,adj_trans~no).
dcg_lexicon([prime],adjective,adj_trans~no).
dcg_lexicon([positive],adjective,adj_trans~no).
dcg_lexicon([transitive],adjective,adj_trans~no).
dcg_lexicon([square],adjective,adj_trans~no).
dcg_lexicon([rational],adjective,adj_trans~no).
dcg_lexicon([irrational],adjective,adj_trans~no).
dcg_lexicon([infinite],adjective,adj_trans~no).

dcg_lexicon([distinct],adjective,adj_trans~from).
dcg_lexicon([disjoint],adjective,adj_trans~from).
dcg_lexicon([parallel],adjective,adj_trans~to).
dcg_lexicon([coprime],adjective,adj_trans~to).

dcg_lexicon([succeeds],verb,transitive~minus..mode~finite..number~singular,succeed).
dcg_lexicon([to,succeed],verb,transitive~minus..mode~to-infinitive,succeed).
dcg_lexicon([succeed],verb,Features,succeed) :-
	Features = transitive~minus,
	( Features = mode~infinitive; Features = number~plural..mode~finite ).
dcg_lexicon([holds],verb,transitive~minus..mode~finite..number~singular,hold).
dcg_lexicon([to,hold],verb,transitive~minus..mode~to-infinitive,hold).
dcg_lexicon([hold],verb,Features,hold) :-
	Features = transitive~minus,
	( Features = mode~infinitive; Features = number~plural..mode~finite ).
dcg_lexicon([does],verb,transitive~minus..mode~finite..number~singular,do).
dcg_lexicon([to,do],verb,transitive~minus..mode~to-infinitive,do).
dcg_lexicon([do],verb,Features,do) :-
	Features = transitive~minus,
	( Features = mode~infinitive; Features = number~plural..mode~finite ).
dcg_lexicon([goes],verb,transitive~minus..mode~finite..number~singular,go).
dcg_lexicon([to,go],verb,transitive~minus..mode~to-infinitive,go).
dcg_lexicon([go],verb,Features,go) :-
	Features = transitive~minus,
	( Features = mode~infinitive; Features = number~plural..mode~finite ).
dcg_lexicon([lies],verb,transitive~minus..mode~finite..number~singular,lie).
dcg_lexicon([to,lie],verb,transitive~minus..mode~to-infinitive,lie).
dcg_lexicon([lie],verb,Features,lie) :-
	Features = transitive~minus,
	( Features = mode~infinitive; Features = number~plural..mode~finite ).

dcg_lexicon([contains],verb,transitive~plus..mode~finite..number~singular,contain).
dcg_lexicon([to,contain],verb,transitive~plus..mode~to-infinitive,contain).
dcg_lexicon([contain],verb,Features,contain) :-
	Features = transitive~plus,
	( Features = mode~infinitive; Features = number~plural..mode~finite ).
dcg_lexicon([divides],verb,transitive~plus..mode~finite..number~singular,divide).
dcg_lexicon([to,divide],verb,transitive~plus..mode~to-infinitive,divide).
dcg_lexicon([divide],verb,Features,divide) :-
	Features = transitive~plus,
	( Features = mode~infinitive; Features = number~plural..mode~finite ).
dcg_lexicon([intersects],verb,transitive~plus..mode~finite..number~singular,intersect).
dcg_lexicon([to,intersect],verb,transitive~plus..mode~to-infinitive,intersect).
dcg_lexicon([intersect],verb,Features,intersect) :-
	Features = transitive~plus,
	( Features = mode~infinitive; Features = number~plural..mode~finite ).

dcg_lexicon([is],verb,transitive~copula..mode~finite..number~singular,be).
dcg_lexicon([are],verb,transitive~copula..mode~finite..number~plural,be).
dcg_lexicon([be],verb,transitive~copula..mode~infinitive,be).
dcg_lexicon([to,be],verb,transitive~copula..mode~to-infinitive,be).

% Temporary hack for "center of"
dcg_lexicon([is, the, center, of],verb,transitive~plus..mode~finite..number~singular,center).
dcg_lexicon([to, be, the, center, of],verb,transitive~plus..mode~to-infinitive,center).
dcg_lexicon([are, centers, of],verb,transitive~plus..mode~finite..number~plural,center).
dcg_lexicon([be, the, center, of],verb,transitive~plus..mode~infinitive,center).


dcg_lexicon([in],preposition,alt_copulas~[]..grouped~no).
dcg_lexicon([on,the,same,side,of],preposition,alt_copulas~[lie]..grouped~1).
dcg_lexicon([on],preposition,alt_copulas~[lie]..grouped~no).
dcg_lexicon([inside],preposition,alt_copulas~[lie]..grouped~no).
dcg_lexicon([through],preposition,alt_copulas~[go]..grouped~no).
dcg_lexicon([between],preposition,alt_copulas~[lie]..grouped~2).


%=============================
% Specifiers, quantifiers and sentence initials.
%=============================

dcg_lexicon([no],specifier,specifier_type~negative).

dcg_lexicon([every],specifier,specifier_type~universal..number~singular).
dcg_lexicon([all],specifier,specifier_type~universal..number~plural).

dcg_lexicon([precisely,one],specifier,specifier_type~one..number~singular).

dcg_lexicon([the],specifier,specifier_type~definite).

dcg_lexicon([a],specifier,specifier_type~indefinite..number~singular).
dcg_lexicon([an],specifier,specifier_type~indefinite..number~singular).
dcg_lexicon([some],specifier,specifier_type~indefinite).
dcg_lexicon([],specifier,specifier_type~indefinite..number~plural).


dcg_lexicon([for,every],quantifier,type~universal..number~singular).
dcg_lexicon([for,all],quantifier,type~universal..number~plural).

dcg_lexicon([there,is],quantifier,type~existential..mode~finite..number~singular).
dcg_lexicon([there,are],quantifier,type~existential..mode~finite..number~plural).
dcg_lexicon([there,exists],quantifier,type~existential..mode~finite..number~singular).
dcg_lexicon([there,exist],quantifier,type~existential..mode~finite..number~plural).
dcg_lexicon([there,be],quantifier,type~existential..mode~infinitive).
dcg_lexicon([there,exist],quantifier,type~existential..mode~infinitive).
dcg_lexicon([there,to,be],quantifier,type~existential..mode~to-infinitive).
dcg_lexicon([there,to,exist],quantifier,type~existential..mode~to-infinitive).


dcg_lexicon([it,is,false],sentence_init,type~negative..mode~finite).
dcg_lexicon([it,be,false],sentence_init,type~negative..mode~infinitive).
dcg_lexicon([it,to,be,false],sentence_init,type~negative..mode~to-infinitive).
dcg_lexicon([it,is,not,the,case],sentence_init,type~negative..mode~finite).
dcg_lexicon([it,not,be,the,case],sentence_init,type~negative..mode~infinitive).
dcg_lexicon([it,not,to,be,the,case],sentence_init,type~negative..mode~to-infinitive).
dcg_lexicon([it,is,the,case],sentence_init,type~affirmative..mode~finite).
dcg_lexicon([it,be,the,case],sentence_init,type~affirmative..mode~infinitive).
dcg_lexicon([it,to,be,the,case],sentence_init,type~affirmative..mode~to-infinitive).

%=====================
% Triggers
%=====================

dcg_lexicon([then],trigger,type~statement..comma~no).

dcg_lexicon(X,trigger,type~statement..comma~Comma):-
	( Type = all; Type = ie; Type = conjunction; Type = conseq ),
	dcg_lexicon(X,trigger,type~Type..comma~Comma).

dcg_lexicon(X,trigger,type~conseq_conjunct..comma~Comma):-
	( Type = all; Type = conseq ),
	dcg_lexicon(X,trigger,type~Type..comma~Comma),
	\+ X = [].

dcg_lexicon([hence],trigger,type~all..comma~optional).
dcg_lexicon([therefore],trigger,type~all..comma~optional).
dcg_lexicon([recall,that],trigger,type~all..comma~no).
dcg_lexicon([now,recall,that],trigger,type~all..comma~no).
dcg_lexicon([now,observe,that],trigger,type~all..comma~no).
dcg_lexicon([now],trigger,type~all..comma~optional).
dcg_lexicon([now,this,implies,that],trigger,type~all..comma~no).
dcg_lexicon([now,this,in,turn,implies,that],trigger,type~all..comma~no).
dcg_lexicon([now,this,implies],trigger,type~all..comma~no).
dcg_lexicon([now,this,in,turn,implies],trigger,type~all..comma~no).
dcg_lexicon([so],trigger,type~all..comma~no).

dcg_lexicon([clearly],trigger,type~conseq..comma~optional).
dcg_lexicon([trivially],trigger,type~conseq..comma~optional).
dcg_lexicon([obviously],trigger,type~conseq..comma~optional).
dcg_lexicon([in,particular],trigger,type~conseq..comma~optional).
dcg_lexicon([observe,that],trigger,type~conseq..comma~no).
dcg_lexicon([furthermore],trigger,type~conseq..comma~optional).
dcg_lexicon([this,implies,that],trigger,type~conseq..comma~no).
dcg_lexicon([this,in,turn,implies,that],trigger,type~conseq..comma~no).
dcg_lexicon([this,implies],trigger,type~conseq..comma~no).
dcg_lexicon([this,in,turn,implies],trigger,type~conseq..comma~no).
dcg_lexicon([finally],trigger,type~conseq..comma~optional).
dcg_lexicon([also],trigger,type~conseq..comma~optional).
dcg_lexicon([],trigger,type~conseq..comma~no).

dcg_lexicon(['i.e.'],trigger,type~ie..comma~no).
dcg_lexicon([so],trigger,type~ie..comma~no).

dcg_lexicon([and],trigger,type~conjunction..comma~no).
dcg_lexicon([but],trigger,type~conjunction..comma~no).

dcg_lexicon([','],trigger,type~conjunction_or_comma..comma~no).
dcg_lexicon(X,trigger,type~conjunction_or_comma..comma~Comma) :-
	dcg_lexicon(X,trigger,type~conjunction..comma~Comma).


dcg_lexicon([we,have],trigger,type~formula..mode~Mode..comma~no) :-
	\+ Mode = infinitive.
dcg_lexicon([we,get],trigger,type~formula..mode~finite..comma~no).
dcg_lexicon([],trigger,type~formula..comma~no).

dcg_lexicon([assume],trigger,type~ass..mode~finite..comma~no).
dcg_lexicon([suppose],trigger,type~ass..mode~finite..comma~no).
dcg_lexicon([assume,that],trigger,type~ass..mode~finite..comma~no).
dcg_lexicon([suppose,that],trigger,type~ass..mode~finite..comma~no).
dcg_lexicon([assume,for,a,contradiction,that],trigger,type~ass..mode~finite..comma~no).
dcg_lexicon([now,assume],trigger,type~ass..mode~finite..comma~no).
dcg_lexicon([now,suppose],trigger,type~ass..mode~finite..comma~no).
dcg_lexicon([now,assume,that],trigger,type~ass..mode~finite..comma~no).
dcg_lexicon([now,suppose,that],trigger,type~ass..mode~finite..comma~no).
dcg_lexicon([now,assume,for,a,contradiction,that],trigger,type~ass..mode~finite..comma~no).

dcg_lexicon([let],trigger,type~ass..mode~infinitive..comma~no).
dcg_lexicon([now,let],trigger,type~ass..mode~infinitive..comma~no).

dcg_lexicon([consider],trigger,type~ass..mode~to-infinitive..comma~no).
dcg_lexicon([now,consider],trigger,type~ass..mode~to-infinitive..comma~no).

dcg_lexicon([consider],trigger,type~variable_declaration..comma~no).
dcg_lexicon([consider,arbitrary],trigger,type~variable_declaration..comma~no).
dcg_lexicon([fix],trigger,type~variable_declaration..comma~no).
dcg_lexicon([fix,arbitrary],trigger,type~variable_declaration..comma~no).
dcg_lexicon([now,consider],trigger,type~variable_declaration..comma~no).
dcg_lexicon([now,consider,arbitrary],trigger,type~variable_declaration..comma~no).
dcg_lexicon([now,fix],trigger,type~variable_declaration..comma~no).
dcg_lexicon([now,fix,arbitrary],trigger,type~variable_declaration..comma~no).


dcg_lexicon([thus],trigger,type~ass_closing..comma~no).

dcg_lexicon([in,all,cases],trigger,type~case_closing..comma~optional).
dcg_lexicon([in,both,cases],trigger,type~case_closing..comma~optional).

