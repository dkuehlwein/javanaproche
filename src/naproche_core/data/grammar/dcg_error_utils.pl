% This file is part of the Attempto Parsing Engine (APE).
% Copyright 2008, Attempto Group, University of Zurich (see http://attempto.ifi.uzh.ch).
%
% The Attempto Parsing Engine (APE) is free software: you can redistribute it and/or modify it
% under the terms of the GNU Lesser General Public License as published by the Free Software
% Foundation, either version 3 of the License, or (at your option) any later version.
%
% The Attempto Parsing Engine (APE) is distributed in the hope that it will be useful, but WITHOUT
% ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
% PURPOSE. See the GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public License along with the Attempto
% Parsing Engine (APE). If not, see http://www.gnu.org/licenses/.


:- module(dcg_error_utils, [
		word//1,                      % ?Word
		word//2,                      % ?Word, +Condition
		word_initial//1,              % ?Word
		word_initial//2,              % ?Word, +Condition
		word_noninitial//1,           % ?Word
		word_noninitial//2,           % ?Word, +Condition
		word_capitalize//2,           % +Word, +WordInitial
		words//1,                     % +WordList
		words//2,                     % +WordList, +Condition
		words_initial//1,             % +WordList
		words_initial//2,             % +WordList, +Condition
		words_noninitial//1,          % +WordList
		words_noninitial//2,          % +WordList, +Condition
		warning//4,                   % +Type, +SentenceID, +Subject, +Description
		try//2,                       % +Goal, error(+Type, +SentenceID, +Subject, +Description)
		reset_progress_record/2,      % +TokenList
		call_position/3,
		record_position/3,
		write_errors/1,
		get_unparsed_tokens_number/2  % -Number
	]).
/*
:- use_module('../logger/error_logger', [
		add_warning_message_once/4,
		add_error_message_once/4
	]).
:- use_module('../lexicon/lexicon_interface').
:- use_module('../lexicon/functionwords').
:- use_module('../lexicon/chars').
:- use_module('../lexicon/is_in_lexicon').
*/

/** <module> Word-level Grammar Rules

This module contains word-level grammar rules. It manages the fact that certain words can be
capitalized at the beginning of a sentence. Furthermore, it keeps track of the parsing process
and, in the case of an error, it can determine up to which token parsing succeeded.

@author Tobias Kuhn
*/


%% word(?Word)
%
% This rule reads the token Word which can be in sentence-initial position.

word(Word) -->
    [Word],
    record_position.

word(Word) -->
    [^, Word],
    record_position.


%% word(?Word, +Condition)
%
% This rule reads the token Word (which can be in sentence-initial position) if the given condition
% is fulfilled.

word(Word, Condition) -->
    [Word],
    { call(Condition) },
    record_position.

word(Word, Condition) -->
    [^, Word],
    { call(Condition) },
    record_position.


%% word_initial(?Word)
%
% This rule reads the token Word in sentence-initial position.

word_initial(Word) -->
    [^, Word],
    record_position.


%% word_initial(?Word, +Condition)
%
% This rule reads the token Word in sentence-initial position if the given condition is fulfilled.

word_initial(Word, Condition) -->
    [^, Word],
    { call(Condition) },
    record_position.


%% word_noninitial(?Word)
%
% This rule reads the token Word if it is not in sentence-initial position.

word_noninitial(Word) -->
	[Word],
    record_position.


%% word_noninitial(?Word, +Condition)
%
% This rule reads the token Word if it is not in sentence-initial position and if the given condition
% is fulfilled.

word_noninitial(Word, Condition) -->
	[Word],
    { call(Condition) },
    record_position.


%% word_capitalize(+Word, +WordInitial)
%
% This rule reads the token Word. In sentence-initial position also WordInitial is accepted.

word_capitalize(Word, _WordInitial) -->
    [Word],
    record_position.

word_capitalize(Word, _WordInitial) -->
	[^, Word],
    record_position.

word_capitalize(_Word, WordInitial) -->
	[^, WordInitial],
    record_position.


%% words(+WordList)
%
% This rule reads the tokens of WordList which can be in sentence-initial position.

words(WordList) -->
    words_noninitial(WordList).

words(WordList) -->
    [^],
    words_noninitial(WordList).


%% words(+WordList, +Condition)
%
% This rule reads the tokens of WordList (which can be in sentence-initial position) if the
% condition is fulfilled.

words(WordList, Condition) -->
    words_noninitial(WordList, Condition).

words(WordList, Condition) -->
    [^],
    words_noninitial(WordList, Condition).


%% words_initial(+WordList)
%
% This rule reads the tokens of WordList if they are in sentence-initial position.

words_initial(WordList) -->
    [^],
    words_noninitial(WordList).


%% words_initial(+WordList, +Condition)
%
% This rule reads the tokens of WordList if they are in sentence-initial position and if the
% condition is fulfilled.

words_initial(WordList, Condition) -->
    [^],
    words_noninitial(WordList, Condition).


%% words_noninitial(+WordList)
%
% This rule reads the tokens of WordList if they are not in sentence-initial position.

words_noninitial([]) -->
	record_position.

wordss_noninitial([Word|Rest]) -->
    [Word],
    words_noninitial(Rest).


%% words_noninitial(+WordList, +Condition)
%
% This rule reads the tokens of WordList if they are not in sentence-initial position and if the
% condition is fulfilled.

words_noninitial([], Condition) -->
    { call(Condition) },
	record_position.

words_noninitial([Word|Rest], Condition) -->
    [Word],
    words_noninitial(Rest, Condition).


%% warning(+Type, +SentenceID, +Subject, +Description)
%
% This predicate can be used as a DCG rule. It reads no token but asserts a warning message.

warning(Type, SentenceID, Subject, Description) -->
	get_positive_position(Pos),
	{
		PrevPos is Pos - 1,
		add_warning_message_once(Type, SentenceID-PrevPos, Subject, Description)
	}.

/*
%% try(+Goal, error(+Type, +SentenceID, +Subject, +Description))
%
% This predicate can be used as a DCG rule. It tries to call the goal. If this fails then an error message
% is asserted and the complete predicate fails.
*/
try(Goal, _, Tokens, Tokens) :-
	call(Goal),
	!.

try(_, error(Type, SentenceID, Subject, Description)) -->
	get_positive_position(Pos),
	{
		PrevPos is Pos - 1,
		add_error_message_once(Type, SentenceID-PrevPos, Subject, Description),
		fail
	}.


%% tokencount(-TokenCount)
%
% This predicate stores the overall number of tokens.

:- dynamic tokencount/1.


%% reset_progress_record(+Type,+TokenList)
%
% This predicate resets the record about how far the parser proceeded in the token list. Furthermore, it
% initializes the record for the new token list.
%
% @param Type can be either "sentences" (for counting how many sentences have not been parsed) or a number indicating inside which sentence we are counting the words that are not yet parsed.

reset_progress_record(Type,TokenList) :-
    retractall(position(_,_)),
    record_position(Type,TokenList, TokenList),
    retractall(tokencount(_)),
    length(TokenList, Length),
    assert(tokencount(Length)),
    !.


%% get_unparsed_tokens_number(+Type,-Number)
%
% This predicate returns the smallest number of tokens that were not parsed (since the record was reset).
%
% @param Type can be either "sentences" (for counting how many sentences have not been parsed) or a number indicating inside which sentence we are counting the words that are not yet parsed.

get_unparsed_tokens_number(Type,Number) :-
    position(Type,First),
    findall(P, position(Type,P), Positions),
    get_minimum(Positions, First, Number).


%% position(+Type,-Position)
%
% This predicate stores the positions, i.e. number of tokens that are not (yet) parsed.
%
% @param Type can be either "sentences" (for counting how many sentences have not been parsed) or a number indicating inside which sentence we are counting the words that are not yet parsed.

:- dynamic position/2.


%% get_positive_position(-Position)
%
% This predicate can be used as a DCG rule. It reads no token but returns the position in a positive
% way, i.e. starting from the beginning of the list not from the end.

get_positive_position(Position, Tokens, Tokens) :-
	length(Tokens, Length),
	tokencount(TokenCount),
	Position is TokenCount - Length,
	!.


%% call_position(-Position, +ListIn, ?ListOut)
%
% This predicate can be used as a DCG rule. Is reads nothing, but tells you how many tokens haven't been parsed so far.

call_position(Position,List, List) :-
    length(List, Position).


%% record_position(+Type, +ListIn, ?ListOut)
%
% This predicate can be used as a DCG rule. Is reads nothing, but records the position.

record_position(Type,List, List) :-
    length(List, Length),
    record_position(Type,Length).


%% record_position(+Type,+Pos)
%
% This predicates records the position Pos (which is the number of unparsed tokens) if it is not
% already recorded.

record_position(Type,Pos) :-
    position(Type,Pos),
    !.

record_position(Type,Pos) :-
    assert(position(Type,Pos)).


%% get_minimum(+List, +TempMin, -Min)
%
% Returns the minimal value of the list or TempMin, whichever is smaller.

get_minimum([], M, M).

get_minimum([N|Rest], Temp, M) :-
    N < Temp,
    !,
    get_minimum(Rest, N, M).

get_minimum([_|Rest], Temp, M) :-
    get_minimum(Rest, Temp, M).

%% write_errors(+Text)
%
% Writes the relevant error messages when dcg_error:naproche_text fails.

write_errors(Text):-
	get_unparsed_tokens_number(sentences,UnparsedSentencesNumber),
	length(Text,TextLength),
	FailedSentenceNumber is TextLength - UnparsedSentencesNumber + 1,
	nth1(FailedSentenceNumber,Text,FailedSentence),
%	FailedSentence = sentence(FailedSentenceId,FailedSentenceContent),
	FailedSentence = sentence(FailedSentenceId,_),
	get_unparsed_tokens_number(UnparsedSentencesNumber,UnparsedWordsNumber),
%	length(UnparsedWords, UnparsedWordsNumber),
%	append(ParsedWords, UnparsedWords, FailedSentenceContent),
%	append(ParsedWords, ['<>'|UnparsedWords], MarkedFailedSentence),
%	add_error_message_once(grammarError,naproche_text,[FailedSentenceId,MarkedFailedSentence],'This is the first sentence that could not be parsed. The sign <> indicates the position where parsing failed.').
	add_error_message_once(grammarError,naproche_text,[FailedSentenceId,UnparsedWordsNumber],'This is the first sentence that could not be parsed. The highlighted word is the first word that could not be parsed.').

