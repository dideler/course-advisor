% Native - a simple shell for use with Prolog
% knowledge bases.  It includes expanations.

% Modified to work with Sicstus Prolog 4.0.
% (B. Ross, Jan 2008)
%
% Modified for assignment 2 of COSC 4P79
% (D. Ideler, Feb 2012)

:- consult(op). % Load op.pl for user-defined operators.
:-op(900,xfy, :).

?- dynamic known/3.

?- unknown(_, fail).

main :-
	greeting,
	repeat,
	write('> '),
	read(X),
	do(X),
  (X == quit; X == halt; X == exit). % More user-friendly halting.

greeting :-
	write('This is the native Prolog shell.'), nl,
	native_help.

do(help) :- native_help, !.
do(load) :- load_kb, !.
do(solve) :- solve, !.
do(trace) :- trace_rules, !.
do(dump) :- !, dump. % Cut before dump so do(X) doesn't execute.
do(how(Goal)) :- how(Goal), !.
do(whynot(Goal)) :- whynot(Goal), !.
do(quit).
do(halt).
do(exit).
do(X) :-
	write(X),
	write(' is not a legal command.'), nl,
	fail.

native_help :-
	write('Type help. load. solve. trace. dump. how(Goal). whynot(Goal). or quit.'),nl,
	write('at the prompt. You can type why. during solve if curious.'), nl.

load_kb :-
	write('Enter file name in single quotes (ex. ''birds.nkb''.): '),
	read(F),
	my_consult(F).

solve :-
	abolish(known/3),
	prove(top_goal(X),[],0), % TODO: consider renaming top_goal to checking_for or possible_answer.
	write('The answer is '),write(X),nl.
solve :-
	write('No answer found.'),nl.

trace_rules :-
    known(trace,on), % if flag is set
    retract(known(trace,on)), % remove set flag
    asserta(known(trace,off)), % add unset flag
    writeln('Trace off.'),
    !. % and dont look any further
trace_rules :-
    known(trace,off), % else if flag is unset
    retract(known(trace,off)), % remove unset flag
    asserta(known(trace,on)), % add set flag
    writeln('Trace on.'),
    !.%, fail.
trace_rules :- % if we get here, it is the first time trace is called.
	asserta(known(trace,on)),
    writeln('Trace on.'),
    !.%, fail.

% Reads in the name of a rule (w/o args) then prints out all the rules for it.
dump :-
    write('Enter rule name without arguments (ex. ''order''.): '),
    read(Rule),
    %atom_concat(Rule,'(_)',GoalAtom), % Goal is atom, so variable won't unify.
    atom_concat(Rule,' iz _',GoalAtom), % TODO decide which to use...
    atom_to_term(GoalAtom,Goal,_), % Converts Goal to a term, variable unifies.
    !,
    clause(Goal,Body),
    write(Goal), writeln(' if '), tab(4),
    dump_rule(Body),
    nl, fail. % Forces backtracking, tries to find more rules with same name.

% If there are multiple facts, split and write them nicely.
dump_rule((A,B)) :- 
    !, dump_rule(A), write(' and '), dump_rule(B).
% Else write the single fact.
dump_rule(Body) :-
    write(Body).

% "ask" asks the user for a yes or no answer to the question.
% TODO: find out why multivalued does and is being used here.

ask(Attribute,Value,_) :-
	known(yes,Attribute,Value),     % Succeed if it's known.
	!. 

ask(Attribute,Value,_) :-
	known(_,Attribute,Value),       % Otherwise fail.
	!, fail.

ask(Attribute,_,_) :-
	\+ multivalued(Attribute),
	known(yes,Attribute,_),         % fail if its some other value.
	!, fail.                        % the cut in clause #1 ensures
					                        % this is the wrong value
ask(A,V,Hist) :-
	write(A :V),                    % If we get here, we need to ask.
	write('? (yes or no) '),
	get_user(Y,Hist),nl,            % get the answer
	asserta(known(Y,A,V)),          % remember it so we dont ask again.
	Y = yes.                        % succeed or fail based on answer.

% "menuask" is like ask, only it gives the user a menu to to choose
% from rather than a yes or no answer. In this case there is no
% need to check for a negative since "menuask" ensures there will
% be some positive answer.

menuask(Attribute,Value,_,_) :-
	known(yes,Attribute,Value),     % succeed if we know
	!.
menuask(Attribute,_,_,_) :-
	known(yes,Attribute,_),         % fail if its some other value
	!, fail.

menuask(Attribute,AskValue,Menu,Hist) :-
	write('What is the value for '),write(Attribute),write('?'),nl,
	display_menu(Menu),
	write('Enter the number of choice> '),
	get_user(Num,Hist),nl,
	pick_menu(Num,AnswerValue,Menu),
	asserta(known(yes,Attribute,AnswerValue)),
	AskValue = AnswerValue.         % succeed or fail based on answer

display_menu(Menu) :-
	disp_menu(1,Menu), !.             % make sure we fail on backtracking

disp_menu(_,[]).
disp_menu(N,[Item | Rest]) :-            % recursively write the head of
	write(N),write('  : '),write(Item),nl, % the list and disp_menu the tail
	NN is N + 1,
	disp_menu(NN,Rest).

pick_menu(N,Val,Menu) :-
	integer(N),                     % make sure they gave a number
	pic_menu(1,N,Val,Menu), !.      % start at one
pick_menu(Val,Val,_).             % if they didn't enter a number, use
	                                % what they entered as the value

pic_menu(_,_,none_of_the_above,[]).  % if we've exhausted the list
pic_menu(N,N, Item, [Item|_]).       % the counter matches the number
pic_menu(Ctr,N, Val, [_|Rest]) :-
	NextCtr is Ctr + 1,                % try the next one
	pic_menu(NextCtr, N, Val, Rest).

get_user(X,Hist) :- % Y
	repeat,
	write('> '),
	read(X),
	process_ans(X,Hist), !.

% Case for handling 'why' during ask prompt.
% FIXME: 'why' messes up user input on backtracking!
% TODO: Try to find a case where it screws up.
process_ans(why,Hist) :-
	write_rule(4,Hist), !, fail.
process_ans(X,_).

% Prolog in Prolog for explanations.
% It is a bit confusing because of the ambiguous use of the comma, both
% to separate arguments and as an infix operator between the goals of a clause.

% prove(Goal, How). This is the equivalent of solve/1 in part 1 of the assignment.
% Goal is the Prolog goal to be proved.
% How is a term building the final proof.
prove(true,_,_) :- !. % Base case, goal proven.
prove(menuask(X,Y,Z),Hist,_) :-
    menuask(X,Y,Z,[menuask(X,Y,Z)|Hist]), !. % Call directly & save in history.
prove(ask(X,Y),Hist,_) :-
    ask(X,Y,[ask(X,Y)|Hist]), !. % Ask and save in history.
prove((Goal,Rest),Hist,N) :- % Multiple goals.
	prove(Goal,Hist,N), % Solve current goal.
	prove(Rest,Hist,N). % Solve the next goal. +1
prove((_,_),_,_) :- !, fail.
prove(Goal,Hist,N) :- % Single goal with trace.
    known(trace,on),
    (tab(N), write('Solving '), write(Goal), writeln('...') ; tab(N), write(Goal), writeln(' fails.'), !, fail),
	clause(Goal,Body),
	prove(Body,[Goal|Hist],N+2), % Also increase # of tabs for inference depth.
    tab(N),write('Solved '), write(Goal),write('.'),nl,nl. % Print solved when solved.
prove(Goal,Hist,_) :- % Single goal without trace.
	clause(Goal,Body),
	prove(Body,[Goal|Hist],_).

% Explanations

% The Why term is different from the How term.
% Why contains the history of the search, which is built up inside outwards.
% Why is relevant when the user is asked for input.
% Why contains a nested set of all the goals that the expert system is trying to solve,
% which justifies why the question was asked.
%
% How is developed outside inwards and contains the final proof when a goal is solved.
% The purpose of How is the final explanation.

% FIXME: Original author didn't finish the 'how' functionality.
how(Goal) :-
	clause(Goal,Body),
	prove(Body,[],_),
	write_body(4,Body).

whynot(Goal) :-
	clause(Goal,Body),
	write_line([Goal,'fails because: ']),
	explain(Body).
whynot(_).

explain(true).
explain((Head,Body)) :-
	!,
	check(Head),
	explain(Body).
explain(Goal) :-   		% new!
	check(Goal).

check(H) :- prove(H,[],_), write_line([H,succeeds]), !.
check(H) :- write_line([H,fails]), fail.

% Writes a single rule of history every time user asks 'why'.
write_rule(N,[]) :-
   tab(N),writeln('because that was the original problem!'),
   %get_user(X,[],Y). % In case the user asks why again, repeat answer.
   read(X),
   process_ans(X,[]). 
write_rule(N,[H|T]) :-
   tab(N),write('because '),writeln(H),
   %get_user(X,T,Y).
   read(X),
   process_ans(X,T).

write_list(N,[]).
write_list(N,[H|T]) :-
	tab(N),write(H),nl,
	write_list(N,T).

write_body(N,(First,Rest)) :-
	tab(N),write(First),nl,
	write_body(N,Rest).
write_body(N,Last) :-
	tab(N),write(Last),nl.

write_line(L) :-
	flatten(L,LF),
	write_lin(LF).
	
write_lin([]) :- nl.
write_lin([H|T]) :-
	write(H), tab(1),
	write_lin(T).

flatten([],[]) :- !.
flatten([[]|T],T2) :-
	flatten(T,T2), !.
flatten([[X|Y]|T], L) :-
	flatten([X|[Y|T]],L), !.
flatten([H|T],[H|T2]) :-
	flatten(T,T2).
                                                                    
my_consult(Files) :-
	load_files(Files, [load_type(source),compilation_mode(assert_all)]).

%tab(N) :- N =< 0, !.
%tab(N) :-
%	write(' '),
%	M is N-1,
%	!,
%	tab(M).


