% Native - a simple shell for use with Prolog knowledge bases.
%
% Initial release.
% @author unknown
%
% Modified to work with Sicstus Prolog 4.0. (Jan 2008)
% @author bross@brocku.ca (Brian Ross)
%
% Modified for COSC 4P79 assignment 2. (Feb 2012)
% @author ideler.dennis@gmail.com (Dennis Ideler)
%
% Modified for COSC 4P79 assignment 3. (Mar 2012)
% @author ideler.dennis@gmail.com (Dennis Ideler)
%
% Modified for COSC 4P79 project. (May 2012)
% @author ideler.dennis@gmail.com (Dennis Ideler)
% @author kelly.moylan@gmail.com (Kelly Moylan)

:- consult(op). % Load op.pl for user-defined operators.
:-op(900,xfy, :).

?- dynamic known/3.

?- unknown(_, fail).

:- nl,write('Type "main." to get started.'),nl,nl.

main :-
  asserta(known(kb,no)),
  greeting,
  repeat,
  write('> '),
  read(X),
  do(X),
  (X == quit; X == halt; X == exit), % User-friendly termination.
  abolish(known/3), abolish(known/2), abolish(known/1), % Clear any knowns.
  !.

greeting :-
  write('Unofficial Brock University Computer Science Course Advisor.'),nl,
  write('This follows the 2012-2013 undegraduate calendar.'),nl,
  write('Built on top of the native Prolog shell.'),nl,nl,
  native_help.

% do/1 commands are expected at the main menu.
% Cut before each command so no other do(_) is attempted after failure.
do(help) :- !, native_help.
do(load) :- !, load_kb.
do(solve) :- !, solve.
do(trace) :- !, trace_rules.
do(dump) :- !, dump.
do(how) :- !, how.
do(why) :- !, write('why only works during inference. Use ''solve'' first.'),nl.
do(whynot(Goal)) :- !, whynot(Goal).
do(quit).
do(halt).
do(exit).
do(X) :-
  write(X),
  write(' is not a legal command. Type ''help.'' to see legal commands.'),nl,
  fail.

native_help :-
  write('Commands:'),nl,
  write('    help. Shows this command menu.'),nl,
  write('    load. Loads a given knowledge base.'),nl,
  write('    solve. Starts the inference.'),nl,
  write('    trace. Toggles tracing the execution of the inference tree.'),nl,
  write('    dump. Dumps the given rules in the knowledge base.'),nl,
  write('    how. Explains why the most recent answer was chosen.'),nl,
  write('    whynot(Goal). Explains why Goal was not chosen.'),nl,
  write('    why. Explains why the current question is being asked.'),nl,
  write('    quit. Terminates the program. ''halt.'' and ''exit.'' also work.'),nl.

load_kb :-
  expand_file_name('*.nkb', Files),
  write('Found the following knowledge bases:'),nl,
  write_list(2,Files),%nl,
  write('Enter a file name in single quotes (ex. ''filename.nkb''.): '),
  read(File),
  ( exists_file(File) ->
      load_files(File, [load_type(source),compilation_mode(assert_all)]),
      write(File),write(' loaded successfully.'),nl,
      retract(known(kb,no)),
      asserta(known(kb,yes));
      write(File),write(' does not exist. Is the spelling and file path correct?'),nl
  ).

solve :-
  known(kb,yes), % Only continue if a KB has been loaded.
  abolish(known/3),
  prove(top_goal(X),[],0),
  courses(X,Courses), % Use the list of courses that belongs to the answer.
% write('The answer is '),write(X),nl, % TODO: remove
% write(Courses),nl, % TODO: remove
  write('That''s it! Here are the courses you should consider taking:'),nl,nl,
  write_advice(Courses),nl,
  abolish(known/1), % Get rid of any previous answers.
  asserta(known(X)).
solve :-
  known(kb,no),
  write('You must load a knowledge base before you can solve.'), nl,
  !.
solve :-
  write('No answer found. Is the knowledge base fully populated?'),nl.

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
  !.
trace_rules :- % if we get here, it is the first time trace is called.
  asserta(known(trace,on)),
  writeln('Trace on.'),
  !.

% Reads in the name of a rule (w//o args) then prints out all the rules for it.
dump :-
  known(kb,yes),
  write('Enter rule name without arguments (ex. degree.): '),
  read(Rule),
  atom_concat(Rule,'(_)',GoalAtom), % Goal is atom, so variable won't unify.
  %atom_concat(Rule,' iz _',GoalAtom), % TODO: switch to this after KB is english-ified
  atom_to_term(GoalAtom,Goal,_), % Converts Goal to a term, variable unifies.
  !,
  clause(Goal,Body),
  write(Goal), writeln(' if '), tab(4),
  dump_rule(Body),
  nl, fail. % Forces backtracking, tries to find more rules with same name.
dump :-
  write('You must load a knowledge base before you can dump.'),nl.

dump_rule((A,B)) :- % If there are multiple facts, split and write them nicely.
  !, dump_rule(A), write(' and '), dump_rule(B).
dump_rule(Body) :-  % Else write the single fact.
  write(Body).

% Recurvisely ask a list of questions.
% NOTE: A dirty way of enforcing prereqs, can improve it later.
ask_list(_,[],_).

% If we're on the last item, check if prereqs met before unneedlessly asking.
ask_list(A,[V|Rest],H) :- 
  Rest = [],
  retractall(credit(yes)), % Remove all yes answers.
  ( credit(no), asserta(known(no,A,V)); % Prereq(s) not met, auto-answer 'no'.
    ask(A,V,H) % Safe to ask (will ask if not known yet).
  ), !.

ask_list(A,[V|Rest],H) :-
  ask(A,V,H),
  ask_list(A,Rest,H).

% "ask" asks the user a yes or no question.

ask(Attribute,Value,_) :-
  known(yes,Attribute,Value),     % Succeed if it's known (i.e. yes).
  !. 

ask(_,Value,_) :-
  known(_,course,Value),          % Succeed if a course is not "yes" (i.e. no).
  asserta(credit(no)),            % Keep track of unmet prereq.
  !.

ask(Attribute,Value,_) :-
  known(_,Attribute,Value),       % But fail for everything else in that case.
  !, fail.

ask(Attribute,_,_) :-             % Also fail if a non-multivalued attribute has
  \+ multivalued(Attribute),      % multivalues (i.e. wrong value).
  known(yes,Attribute,_), 
  !, fail.

ask(A,V,Hist) :-                  % If we get here, we need to ask.
  write('Do you have the credit for '),
  write(A :V),                    
  write('? (yes or no) '),
  get_user(Y,Hist),nl,
  asserta(known(Y,A,V)),          % Remember it so we don't ask again.
  asserta(credit(Y)).

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
  write('Enter the number of choice '),
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

get_user(Y,Hist) :-
  repeat,
  write('> '),
  read(X),
  process_ans(X,Hist,Y), !.

% Case for handling 'why' during ask prompt.
process_ans(why,Hist,Y) :-
	write_list(4,Hist,Rest),
	get_user(Y,Rest).
process_ans(why, [], Y) :-
	write_list(4,[],Rest),
	get_user(Y,Rest).
process_ans(X,_,X).

% Prolog in Prolog for explanations.
% It is a bit confusing because of the ambiguous use of the comma, both
% to separate arguments and as an infix operator between the goals of a clause.

% prove(Goal, How). This is the equivalent of solve/1 in part 1 of the assignment.
% Goal is the Prolog goal to be proved.
% How is a term building the final proof.
prove(true,_,_) :- !. % Base case, goal proven.
prove(menuask(X,Y,Z),Hist,_) :-
  menuask(X,Y,Z,[menuask(X,Y,Z)|Hist]), !. % Call directly & save in history.
prove(ask_list(X,Y),Hist,_) :-
  retractall(credit(_)), % Get rid of any previously saved answers.
  ask_list(X,Y,[ask_list(X,Y)|Hist]), !. % Call directly & save in history.
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
% Why contains a nested set of all the goals that the expert system is trying
% to solve, which justifies why the question was asked.
%
% How is developed outside inwards and contains the final proof when a goal is
% solved. The purpose of How is the final explanation.

how :-
  known(Goal),
  clause(degree(Goal),Body), % Get the body from the proven top goal.
  prove(Body,[],0),
  write_body(4,Body),
  !.
how :-
  write('You must solve a task first.'),nl.

% whynot(Goal) explains why the Goal solution did not succeed.
whynot(Goal) :-  % Cannot solve if user queries with the most recent answer.
  known(Goal),
  write(Goal),write(' was actually the degree used for the course suggestions.'),nl,
  write('Did you mean a different type of degree?'),nl,
  !.
whynot(Goal) :-  % Otherwise, solve if an answer exists and it's not the most recent.
  known(_),
  clause(degree(Goal),Body),
  write_line([Goal,'fails because: ']),!,
  explain(Body).
whynot(_) :-  % Finally, if there is no history of an answer, let them know.
  write('You must solve a task first, or, the entered degree does not exist.'),nl.

explain(true).
explain((Head,Body)) :-
  !,
  check(Head),
  explain(Body).
explain(Goal) :-   		% new!
  check(Goal).

check(H) :- prove(H,[],_), write_line([H,succeeds]), !.
check(H) :- write_line([H,fails]), fail.

% suggest_course() :- TODO: probably better to put logic from write_advice, here.

% Writes the courses that are advised.
write_advice([]).
write_advice([Course_code|Rest]) :-
  known(Credit,course,Course_code),
%  write(Course_code), write(' = '), write(Credit), nl, % TODO: remove
  ( Credit == no, % TODO: safer to suggest course if credit is not yes.
    atom_concat('course(',Course_code,GoalTemp), % TODO: modify once KB is english-ified
    atom_concat(GoalTemp,',Name)',GoalAtom), % Goal is now an atom, won't unify yet.
    atom_to_term(GoalAtom,Goal,_), % Converts Goal to a term, now it can unify.
    clause(Goal,_), tab(2), write(Goal), nl,
    write_advice(Rest) ;
    write_advice(Rest)
  ).

% Writes a single rule of history every time user asks 'why'.
write_list(N,[],[]) :-
	tab(N),
	write('Just answer the question.'),
	nl.
write_list(N,[H],[H]) :- 
	tab(N),
	write('Just answer the question.'),
	nl.
write_list(N,[H|T], T) :-
	tab(N),
	write('Because '),
	write(H),
	nl.

% Write list items on separate lines with a given number of tabs preceding.
write_list(_, []).
write_list(N, [H|T]) :-
  tab(N), write(H), nl,
  write_list(N, T).

% If only the list is provided, print it without tabs.
write_list(L) :- write_list(0,L).

write_body(N,(First,Rest)) :-
  tab(N),write(First),nl,
  write_body(N,Rest).
write_body(N,Last) :-
  tab(N),write(Last),nl.

% Write a list as a line (no commas).
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

%tab(N) :- N =< 0, !.
%tab(N) :-
%	write(' '),
%	M is N-1,
%	!,
%	tab(M).
