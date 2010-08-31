:- begin_tests(cgi_input).
:- set_prolog_flag(character_escapes, false).


test('Simple Sentence without Math') :-
	cgi_input:create_naproche_input('There is a house.',Sentence),
	Sentence  = [sentence(1, [there, 'is', a, house])].

test('Simple Sentence with "i.e."') :-
	cgi_input:create_naproche_input('There is a house, i.e. this sentence is boring.',Sentence),
	Sentence  = [sentence(1, [there, 'is', a, house, (','), 'i.e.', this, sentence, 'is', boring])].

test('Simple Sentence with double backslashes') :-
	cgi_input:create_naproche_input('\\hallo.',Sentence),
	Sentence  = [sentence(1, [hallo])].
	
test('Two Sentences without Math') :-
	cgi_input:create_naproche_input('There is a house. The house is green.',Sentence),
	Sentence = [sentence(1, [there, 'is', a, house]), sentence(2, [the, house, 'is', green])].

test('Simple Sentence with Math') :-
	cgi_input:create_naproche_input('$X=3$.',Sentence),
	Sentence = [sentence(1, [math(['X', =, '3'])])].

test('Two Sentences with Math') :-
	cgi_input:create_naproche_input('$X=3$. $f(Y)=X$ no house.',Sentence),
	Sentence = [sentence(1, [math(['X', =,'3'])]), sentence(2,[math([f,'(','Y', ')', =, 'X']), no, house])].

test('Sentence ending with an :') :-
	create_naproche_input('Die Welt: sie ist ein Suendenpfuhl.', Sentence),
	Sentence = [sentence(1, [die, welt]), sentence(2, [sie, ist, ein, suendenpfuhl])].



test('Simple Sentence without Math, newline at the beginning') :-
        cgi_input:create_naproche_input('
	There is a house.',Sentence),
	        Sentence  = [sentence(1, [there, 'is', a, house])].


test('Error message: Missing terminal symbol') :-
	clear_messages,
	\+cgi_input:create_naproche_input('Die Welt ist schoen',_Sentence),
	get_messages(Messages),
	Messages = [message(error, inputError, create_naproche_input, 'Die Welt ist schoen', 'Could not parse input.'), message(error, inputError, sentence, 0, 'No terminal symbol found.')].


test('Error message: Missing $') :-
	clear_messages,
	\+cgi_input:create_naproche_input('$X=X.',_Sentence),
	get_messages(Messages),
	Messages = [message(error, inputError, create_naproche_input, '$X=X.', 'Could not parse input.'), message(error, inputError, sentence, 0, 'No terminal symbol found.'), message(error, inputError, sentence, '$X=X.', 'Could not parse math mode.'), message(error, inputError, math_fail, 0, 'Missing $.')].


test('Error message: Wrong Latex Code') :-
	clear_messages,
	\+cgi_input:create_naproche_input('$\a $.',_Sentence),
	get_messages(Messages),
	Messages =[message(error, inputError, create_naproche_input, '$\a $.', 'Could not parse input.'), message(error, inputError, sentence, '$\a $', 'Could not parse math mode.'), message(error, inputError, input_math, '\a', 'Latex command not supported.')]. 	


test('Error message: Missing Latex command') :-
	clear_messages,
	\+cgi_input:create_naproche_input('$\.',_Sentence),
	get_messages(Messages),
	Messages = [message(error, inputError, create_naproche_input, $\., 'Could not parse input.'), message(error, inputError, sentence, 0, 'No terminal symbol found.'), message(error, inputError, sentence, $\., 'Could not parse math mode.'), message(error, inputError, math_fail, 0, 'Missing $.'), message(error, inputError, input_math, '', 'Missing Latex command.')].


test('Error message: Missing $, Wrong Latex code, missing .') :-
	clear_messages,
	\+cgi_input:create_naproche_input('$\NoLatexCode ',_Sentence),
	get_messages(Messages),
	Messages = [message(error, inputError, create_naproche_input, '$\NoLatexCode ', 'Could not parse input.'), message(error, inputError, sentence, 0, 'No terminal symbol found.'), message(error, inputError, sentence, '$\NoLatexCode ', 'Could not parse math mode.'), message(error, inputError, math_fail, 0, 'Missing $.'), message(error, inputError, input_math, '\NoLatexCode', 'Latex command not supported.')].



:- end_tests(cgi_input).
