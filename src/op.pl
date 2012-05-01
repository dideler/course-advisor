% http://www.swi-prolog.org/pldoc/doc_for?object=section(2,'4.23',swi('/doc/Manual/operators.html'))

% Operator definitions:
% op(Precedence, Type, Name)
% Precedence is an integer between 0 and 1200. Precedence 0 removes the declaration.
% Type is one of: xf, yf, xfx, xfy, yfx, fy or fx.
% Where f is the functor, x and y are arguments.
% Name can also be a list of names.
:- op(980, fx, if).
:- op(979, xfy, else).
:- op(978, xfx, then).
:- op(950, xfy, or).  % Same as ';' but higher priority.
:- op(940, xfy, and). % Same as ',' but higher priority.
:- op(930, xfy, [iz, are, iS, on]).

% Custom predicates for the defined operators:

(if X then Y else _) :- X, !, Y.
(if _ then _ else Z) :- Z.

or(X,Y) :- X; Y.

and(X,Y) :- X, Y.

/*
(X iz Y) :-
  atom_concat(X,'(',G1),
  atom_concat(G1,Y,G2),
  atom_concat(G2,')',GoalAtom),
  atom_to_term(GoalAtom,Goal,_),
  write('executing '), writeln(Goal),
  Goal.

izz(X,Y) :-
  atom_concat(X,'(',G1),
  atom_concat(G1,Y,G2),
  atom_concat(G2,')',GoalAtom),
  atom_to_term(GoalAtom,Goal,_),
  write('executing '), writeln(Goal),
  Goal.

(X are Y) :-
  atom_concat(X,'(Y)',GoalAtom),
  atom_to_term(GoalAtom,Goal,_),
  Goal.

family(ideler) :- write('aww yeahhh').
family iz ideler :- write('works').
family izz ideler :- write('yahoo').
*/

println(X) :-
  print(X), nl. % Can also use write(X), nl. Or writeln(X).

request(Msg, Input) :-
  print(Msg), read(Input).

% Assert a fact if not already known.
remember(X) :- X, !.
remember(X) :- assert(X).

max(X,Y,Z) :-
  (if X > Y then Z = X else Z = Y).
% ( condition -> then_clause ; else_clause )
