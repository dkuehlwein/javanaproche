:- module(naproche,[naproche_text/5]).

:- use_module(library(pldoc)).
:- use_module(naproche(gulp4swi)).
:- use_module(naproche(dcg_lexicon)).
:- use_module(naproche(fo_grammar)).
:- use_module(naproche(dcg_utils)).
:- use_module(naproche(error_logger)).
:- use_module(naproche(prs)).

:- use_module(library(pldoc)).
:- use_module(library(ugraphs)).

:- use_module(naproche(premises)).
:- use_module(naproche(discharge_obligations)).
:- use_module(naproche(math_lexicon)).
:- use_module(naproche(translation_tptp)).
:- use_module(naproche(graph)).

:- use_module(library(ugraphs)).
:- use_module(naproche(utils)).
:- use_module(naproche(stats)).
:- use_module(naproche(output)).

:- op(601,xfx, user:(=>)).
:- op(601,xfx, user:(==>)).
:- op(601,xfx, user:(:=)).
:- op(601,xfx, user:(<=>)).
:- op(601,xfx, user:(v)).
:- op(601,xfx, user:(<=)).
:- op(699,xfx, user:(::)).

/** <module> High level proof checking predicate
 *
 *  This module sequentially parses the input text (in its preparsed Prolog-readable form), produces the PRS from it, produces proof obligations and checks them.
 *  @author Marcos Cramer
 *  @author Daniel Kühlwein
 *
 */

process_sentences :-
	process_sentence,
	!,
	process_sentences.

process_sentences.


process_sentence :-
	read_data([],_,_,_,_,_).

process_sentence :-
	read_data([sentence(Id,Content)|TextOut],StackIn,SPRS,PremisesIn),
	pm(Content,Id,StackIn,PremisesIn,StackOut,PremisesOut),
	write_data(TextOut,StackOut,SPRS,PremisesOut,NewOut,GraphOut).

%%	pm(+SentenceContent,+Id,+StackIn,+PremisesIn,-StackOut,-PremisesOut)
%
%	pm is the macrostructure parser.

pm(Content,Id,[up|StackTail],PremisesIn,StackOut,PremisesOut) :-
	pm(Content,Id,StackTail,PremisesIn,StackOut,PremisesOut).

pm(Content,Id,[[SentenceGoal]|StackTail],Premises,StackTail,Premises) :-
	sentence_goal(Content,Id,SentenceGoal,PremisesIn),
	!.

pm(Content,Id,[{[Command]}|StackTail],PremisesIn,StackOut,PremisesOut) :-
	process_command(Command,StackTail,PremisesIn,StackTmp,PremisesTmp),
	pm(Content,Id,StackTmp,PremisesTmp,StackOut,PremisesOut).

pm(Content,Id,[{Goals}]|StackTail],PremisesIn,StackOut,PremisesOut) :-
	call(Goals),
	pm(Content,Id,StackTail,PremisesIn,StackOut,PremisesOut).

pm(Content,Id,[CurrentGoal|StackTail],PremisesIn,StackOut,PremisesOut) :-
	--->(CurrentGoal,ExpandedGoals),
	expand_stack(ExpandedGoals,StackTail,StackTmp),
	prepare_prss(CurrentGoal,StackTmp,StackTmp1),
	pm(Content,Id,StackTmp1,PremisesIn,StackOut,PremisesOut).

%%	sentence_goal(+Content,+Id,+SentenceGoal,+PremisesIn)
%
%	This predicate calls the sentence parser.

sentence_goal(Content,Id,Goal,PremisesIn) :-
	Goal =.. GoalList,
	append(GoalList,[Id,PremisesIn,Content,[]],FinalGoalList),
	FinalGoal =.. FinalGoalList,
	call(FinalGoal).

%%	process_command(+Command,+StackIn,+PremisesIn,-StackOut,-PremisesOut)
%
%	This predicate processes a command.

process command(add_cond(PRS,AssPRS ==> ConclPRS),StackIn,PremisesIn,StackOut,PremisesOut) :-
	prepare_stack(StackIn,AssPRS,[],PremisesIn,PremisesAss,StackTmp1),
	prepare_stack(StackTmp1,ConclPRS,[],PremisesAss,PremisesAssConcl,StackTmp2),
	prepare_premises(StackTmp2,PRS,[make_implication_premises(PremisesIn,PremisesAss,PremisesAssConcl,PremisesImpl)],PremisesImpl,PremisesOut,StackOut),
	PRS = prs([Ass ==> Concl|CondMOut],CondMOut,Acc,Acc),
	AssPRS = prs(CondMIn,[],Acc,AccTmp),
	new_prs(Id,Acc,Ass),
	new_prs(conseq(Id),AccTmp,Concl),
	

process_command(add_cond(PRS,SentencePRS),StackIn,PremisesIn,StackOut,PremisesOut) :-
	

%%	prepare_stack(+StackIn,+PRS,+Commands,+PremisesIn,-PremisesOut,-StackOut)
%
%	Prepare stack using prepare_prss und prepare_premises.

prepare_stack(StackIn,PRS,Commands,PremisesIn,PremisesOut,StackOut) :-
	PRS = prs(_,_,_,_),
	prepare_prss(dummy(PRS),StackIn,StackTmp),
	prepare_premises(StackTmp,PRS,Commands,PremisesIn,PremisesOut,StackOut).

%%	prepare_premises(+StackIn,+PRS,+Commands,+PremisesIn,-PremisesOut,-StackOut)
%
%	The stack is prepared in such a way that PremisesIn is the ingoing list of active premises for the first
%	occurence of PRS and PremisesOut is the outgoing list of premises for the last occurence of PRS. In
%	between the active premises are passed over. 
%	The commands are added to the stack list just before the first occurence of PRS.
%	The stack is prepared in this way up to the first occurence of "up".

prepare_premises([up|Tail],_,_,Premises,Premises,[up|Tail]).

prepare_premises([Head|TailIn],PRS,Commands,PremisesIn,PremisesOut,[Head|TailOut]) :-
	\+ occurs(PRS,Head),
	prepare_premises(TailIn,PRS,Commands,PremisesIn,PremisesOut,TailOut).

prepare_premises([Head|TailIn],PRS,Commands,PremisesIn,PremisesOut,StackOut) :-
	make_premises(Head,PremisesIn,PremisesTmp),
	prepare_premises(Tail,PRS,[],PremisesTmp,PremisesOut,TailOut),
	add_commands(Commands,[Head|TailOut],StackOut).

%%	make_premises(+GoalIn,+PremisesIn,+PremisesOut,-GoalOut)
%
%	PremisesIn and PremisesOut are declared the ingoing and outgoing premise lists of Goal.

make_premises({[CommandIn]},PremisesIn,PremisesOut,{[CommandOut]}) :-
	!,
	CommandIn =.. [P|Args],
	CommandOut =.. [P,PremisesIn,PremisesOut|Args].

make_premises([SentenceGoalIn],PremisesIn,PremisesOut,[SentenceGoalOut]) :-
	!,
	SentenceGoalIn =.. [P|Args],
	SentenceGoalOut =.. [P,PremisesIn,PremisesOut|Args].

make_premises(Goal,PremisesIn,PremisesOut,GoalOut) :-
	GoalIn =.. [P|Args],
	GoalOut =.. [P,PremisesIn,PremisesOut|Args].

%%	expand_stack(+Goals,+StackIn,-StackOut)
%
%	Goals is a comma list of goals from --->. Stack is modified so to contain these goals, as well as
%	an "up" at the end of these goals.

expand_stack((Goal,Tail),StackIn,[Goal|StackTmp]) :-
	!,
	expand_stack(Tail,StackIn,StackTmp).

expand_stack(Goal,StackIn,[Goal,up|StackIn]).
	add_goal(Goal,StackIn,StackOut).

%%	prepare_prss(+Goal,+StackIn,-StackOut)
%
%	If the first argument of Goal is of the form "prs(CondMIn,CondMOut,AccMIn,AccMOut)",
%	then all occurences of this term in StackIn are modified so as to pass on the condition
%	and accessible marker from the first to the last of these occurences. (E.g., if there
%	are three such occurences, they become "prs(CondMIn,CondM1,AccMIn,AccM1)", 
%	"prs(CondM1,CondM2,AccM1,AccM2)" and "prs(CondM2,CondMOut,AccM2,AccMOut)".)

prepare_prss(

%%	add_goal(+Goal,+StackIn,-StackOut)
%
%	Adds Goal to the stack.


