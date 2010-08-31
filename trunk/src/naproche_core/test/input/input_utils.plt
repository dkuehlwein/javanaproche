:- begin_tests(input_utils).
:- set_prolog_flag(character_escapes, false).


test('Simple Sentence without Math') :-
	input_utils:create_naproche_input('There is a house.',Sentence),
	Sentence  = [sentence(1, [there, 'is', a, house])].
	
test('Two Sentences without Math') :-
	input_utils:create_naproche_input('There is a house. The house is green.',Sentence),
	Sentence = [sentence(1, [there, 'is', a, house]), sentence(2, [the, house, 'is', green])].

test('Simple Sentence with Math') :-
	input_utils:create_naproche_input('$X=3$.',Sentence),
	Sentence = [sentence(1, [math(['X', =, '3'])])].

test('Two Sentences with Math') :-
	input_utils:create_naproche_input('$X=3$. $f(Y)=X$ no house.',Sentence),
	Sentence = [sentence(1, [math(['X', =,'3'])]), sentence(2,[math([f,'(','Y', ')', =, 'X']), no, house])].

test('Sentence ending with an :') :-
	input_utils:create_naproche_input('Die Welt: sie ist ein Suendenpfuhl.', Sentence),
	Sentence = [sentence(1, [die, welt]), sentence(2, [sie, ist, ein, suendenpfuhl])].

test('Latex with one argument (sqrt)') :-
	input_utils:create_naproche_input('$\sqrt{a}=b$.', Sentence),
	Sentence = [sentence(1, [math([sqrt, '(', a, ')', =, b])])].

test('Latex with two arguments (frac)') :-
	input_utils:create_naproche_input('$\frac{a*b}{b}=a$.', Sentence),
	Sentence = [sentence(1, [math([frac, '(', a, *, b, (,), b, ')', =, a])])].

test('Simple Sentence without Math, newline at the beginning') :-
        input_utils:create_naproche_input('
	There is a house.',Sentence),
	        Sentence  = [sentence(1, [there, 'is', a, house])].

% ---------------------------------------- Error messages -------------------------------------------------------------------
test('Error message: Missing terminal symbol') :-
	error_logger:clear_messages,
	\+input_utils:create_naproche_input('Die Welt ist schoen',_Sentence),
	get_messages(Messages),
	Messages = [message(error, inputError, create_naproche_input, 'Die Welt ist schoen', 'Could not parse input.'), message(error, inputError, sentence, 0, 'No terminal symbol found.')].


test('Error message: Missing $') :-
	error_logger:clear_messages,
	\+input_utils:create_naproche_input('$X=X.',_Sentence),
	get_messages(Messages),
	Messages = [message(error, inputError, create_naproche_input, '$X=X.', 'Could not parse input.'), message(error, inputError, sentence, 0, 'No terminal symbol found.'), message(error, inputError, sentence, '$X=X.', 'Could not parse math mode.'), message(error, inputError, math_fail, 0, 'Missing $.')].


test('Error message: Wrong Latex Code') :-
	error_logger:clear_messages,
	\+input_utils:create_naproche_input('$\a $.',_Sentence),
	get_messages(Messages),
	Messages =[message(error, inputError, create_naproche_input, '$\a $.', 'Could not parse input.'), message(error, inputError, sentence, '$\a $', 'Could not parse math mode.'), message(error, inputError, input_math, '\a', 'Latex command not supported.')]. 	


test('Error message: Missing Latex command') :-
	error_logger:clear_messages,
	\+input_utils:create_naproche_input('$\.',_Sentence),
	get_messages(Messages),
	Messages = [message(error, inputError, create_naproche_input, $\., 'Could not parse input.'), message(error, inputError, sentence, 0, 'No terminal symbol found.'), message(error, inputError, sentence, $\., 'Could not parse math mode.'), message(error, inputError, math_fail, 0, 'Missing $.'), message(error, inputError, input_math, '', 'Missing Latex command.')].


test('Error message: Missing $, Wrong Latex code, missing .') :-
	error_logger:clear_messages,
	\+input_utils:create_naproche_input('$\NoLatexCode ',_Sentence),
	get_messages(Messages),
	Messages = [message(error, inputError, create_naproche_input, '$\NoLatexCode ', 'Could not parse input.'), message(error, inputError, sentence, 0, 'No terminal symbol found.'), message(error, inputError, sentence, '$\NoLatexCode ', 'Could not parse math mode.'), message(error, inputError, math_fail, 0, 'Missing $.'), message(error, inputError, input_math, '\NoLatexCode', 'Latex command not supported.')].



:- end_tests(input_utils).
