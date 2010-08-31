:- begin_tests(expecting_proof_and_qed).


 test('baby theorem') :-
  expecting_proof_and_qed(theorem,42,[sentence(0,[proof]),sentence(0,[qed])],[]),
  !.

 test('ordinary lemma') :-
  expecting_proof_and_qed(lemma,42,[sentence(0,[proof]),sentence(0,lemma),sentence(0,proof),sentence(0,[qed]),sentence(0,qed)],[]),
  !.

 test('long theorem including a lemma') :-
  expecting_proof_and_qed(theorem,42,[sentence(0,[proof]),sentence(0,ho),sentence(0,[lemma]),sentence(0,hoho),sentence(0,[proof]),sentence(0,[qed]),sentence(0,hohoho),sentence(0,[qed])],[]),
  !.

 test('theorem including two lemmas') :-
  expecting_proof_and_qed(theorem,42,[sentence(0,[proof]),sentence(0,[lemma]),sentence(0,[proof]),sentence(0,[qed]),sentence(0,[lemma]),sentence(0,[proof]),sentence(0,[qed]),sentence(0,[qed])],[]),
  !.

 test('one qed missing') :-
  clear_messages,
  \+ expecting_proof_and_qed(theorem,23,[sentence(0,[proof]),sentence(0,[lemma]),sentence(0,[proof]),sentence(0,[qed])],[]),
  get_messages([message(error, textError, expecting_proof_and_qed, 23, 'This theorem is missing a "qed".')]),
  !.

 test('one proof missing') :-
  clear_messages,
  \+ expecting_proof_and_qed(theorem,23,[sentence(0,[lemma]),sentence(0,[proof]),sentence(0,[qed]),sentence(0,[qed])],[]),
  get_messages([message(error, textError, expecting_proof_and_qed, 23, 'This theorem is missing a "proof".')]),
  !.

 test('proof missing in a "long" lemma') :-
  clear_messages,
  \+ expecting_proof_and_qed(lemma,1,[sentence(4,[for,every,element,math([y]),some,set,does,not,contain,math([y])]),sentence(5,[proof]),sentence(6,[let,math([y]),be,an,element]),sentence(7,[then,by,axiom,1,there,is,a,set,math([x]),such,that,math([x]),contains,no,element]),sentence(8,[then,math([x]),does,not,contain,math([y])]),sentence(9,[so,some,set,does,not,contain,math([y])])],[]),
  get_messages([message(error, textError, expecting_proof_and_qed, 1, 'This lemma is missing a "qed".')]),
  !.

 test('lemma including lemma') :-
  clear_messages,
  \+ expecting_proof_and_qed:expecting_proof_and_qed(lemma,3,[sentence(0,[proof]),sentence(0,[lemma]),sentence(0,[proof]),sentence(0,[qed]),sentence(0,[qed])],[]),
  get_messages([message(error, textError, expecting_proof_and_qed, 3, 'Lemmas may not contain lemmas.')]),
  !.

:- end_tests(expecting_proof_and_qed).
