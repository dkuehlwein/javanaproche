:- begin_tests(input_parser).
:- set_prolog_flag(character_escapes, false).


test('Simple Sentence without Math') :-
	input_parser:create_naproche_input('There is a house.',Sentence),
	Sentence  = [sentence(1, 0, 17, [word(0,5), word(6,8), word(9,10), word(11,16)], [there, 'is', a, house])].
	
test('Two Sentences without Math') :-
	input_parser:create_naproche_input('There is a house. The house is green.',Sentence),
	Sentence = [sentence(1, 0, 17, [word(0,5), word(6,8), word(9,10), word(11,16)], [there, 'is', a, house]), sentence(2, 18, 37, [word(18,21), word(22,27), word(28,30), word(31,36)], [the, house, 'is', green])].

test('Simple Sentence with Math') :-
	input_parser:create_naproche_input('$X=3$.',Sentence),
	Sentence = [sentence(1, 0, 6, [math(1,4)], [math(['X', =, '3'])])].

test('Two Sentences with Math') :-
	input_parser:create_naproche_input('$X=3$. $f(Y)=X$ no house.',Sentence),
	Sentence = [sentence(1, 0, 6, [math(1,4)], [math(['X', =, '3'])]), sentence(2,7,25,[math(8, 14), word(16, 18), word(19, 24)], [math([f,'(','Y', ')', =, 'X']), no, house])].

test('Sentence ending with an :') :-
	input_parser:create_naproche_input('Die Welt: sie ist ein Suendenpfuhl.', Sentence),
	Sentence = [sentence(1,0, 9, [word(0,3), word(4,8)], [die, welt]), sentence(2, 10, 35, [word(10,13), word(14,17), word(18,21), word(22,34)], [sie, ist, ein, suendenpfuhl])].

test('Latex with one argument (sqrt)') :-
	input_parser:create_naproche_input('$\sqrt{a}=b$.', Sentence),
	Sentence = [sentence(1, 0, 13, [math(1, 11)], [math(['\sqrt', '(', a, ')', '=', b])])].

test('Latex with two arguments (frac)') :-
	input_parser:create_naproche_input('$\frac{a*b}{b}=a$.', Sentence),
	Sentence = [sentence(1, 0, 18, [math(1,16)], [math(['\frac', '(', 'a', '*', 'b', ',' , 'b', ')', '=', 'a'])])].

test('Simple Sentence without Math, newline at the beginning') :-
        input_parser:create_naproche_input('
There is a house.',Sentence),
		Sentence  = [sentence(1, 1, 18, [word(1, 6), word(7, 9), word(10, 11), word(12, 17)], ['there', 'is', 'a', 'house'])].


% ---------------------------------------- Error messages -------------------------------------------------------------------
test('Error message: Missing terminal symbol') :-
	error_logger:clear_messages,
	\+input_parser:create_naproche_input('Die Welt ist schoen',_Sentence),
	get_messages(Messages),
	Messages = [message(error, inputError, create_naproche_input, 'Die Welt ist schoen', 'Could not parse input.'), message(error, inputError, sentence, 0, 'No terminal symbol found.')].

test('Error message: Missing $') :-
	error_logger:clear_messages,
	\+input_parser:create_naproche_input('$X=X.',_Sentence),
	get_messages(Messages),
	Messages = [message(error, inputError, create_naproche_input, '$X=X.', 'Could not parse input.'), message(error, inputError, sentence, 0, 'No terminal symbol found.'), message(error, inputError, sentence, '$X=X.', 'Could not parse math mode.'), message(error, inputError, math_fail, 0, 'Missing $.')].

test('Error message: Missing Latex command') :-
	error_logger:clear_messages,
	\+input_parser:create_naproche_input('$\ $',_Sentence),
	get_messages(Messages),
	Messages = [message(error, inputError, create_naproche_input, '$\ $', 'Could not parse input.'), message(error, inputError, sentence, 0, 'No terminal symbol found.'), message(error, inputError, sentence, '$\ $', 'Could not parse math mode.'), message(error, inputError, input_math, 'abc', 'Missing Latex command.')].

:- end_tests(input_parser).
