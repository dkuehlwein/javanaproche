:- begin_tests(graph).

/* Deactivated since the predicate is under construction
test(obligation_graph) :-
	graph:obligation_graph([[x,[id~one],y],[x,[id~two,id~three],y]],[],X),
	X = [one-[], three-[two], two-[one]].
*/

:- end_tests(graph).
