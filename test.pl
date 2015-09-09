printHelloWorld :-
  write('Hello world !'),
  nl.

sum([], 0) :- !.
sum([T|Q], Somme) :-
  sum(Q, S),
  Somme is T + S.

writeSum :-
  sum([1,2,3,4,5,6], S),
  write(S), nl.

plusGros('rhinocéros','cheval').
plusGros('cheval', 'chien').
plusGros('chien', 'chat').
plusGros('chat', 'hamster').

estPlusGros(X, Y) :-
  plusGros(X, Y).

estPlusGros(X, Z) :-
  plusGros(X, Y),
  estPlusGros(Y, Z).

phrase(_) :-
  plusGros(X, Y),
  write('Le '), write(X), write(' est plus gros que le '), write(Y), nl.

phrase(_) :-
  nl,
  write('Donc: '), nl.

phrase(_) :-
  estPlusGros(X, Y),
  \+plusGros(X, Y),
  write('Le '), write(X), write(' est plus gros que le '), write(Y), nl.

phrases :-
  findall(X, phrase(X), _).

habite(jean, belfort).
habite(lucie, paris).
habite(christian, toulouse).
habite(adeline, paris).
habite(nicolas, paris).

memeVille(X, Y) :-
  habite(X, VilleX),
  habite(Y, VilleY),
  X \= Y,
  VilleX == VilleY.

enum(X, [T|_], (X,T) ).
enum(X, [_|Q], R) :- enum(X, Q, R).

%factorial

factorial(0,1).

factorial(X, Y):-
  X>0,
  X1 is X-1,
  factorial(X1, Y1),
  Y is Y1*X.

factorial2(0, F, F).

factorial2(N, A, F):-
  N>0,
  A1 is N*A,
  N1 is N-1,
  factorial2(N1, A1, F).

%hanoi puzzle

move(1, X, Y, _):-
  write('move top disc from '),
  write(X),
  write(' to '),
  write(Y),
  nl.

move(N, X, Y, Z):-
  N>1,
  N1 is N-1,
  move(N1, X, Z, Y),
  move(1, X, Y, Z),
  move(N1, Z, Y, X).

edit(File) :-
  name(File, FileString),
  name('gvim ', TextEditString),
  append(TextEditString, FileString, EDIT),
  name(E, EDIT),
  shell(E).

%tree database
:- op(500, xfx, 'is_parent').

a is_parent b.    c is_parent g.     f is_parent l.     j is_parent q. 
a is_parent c.    c is_parent h.     f is_parent m.     j is_parent r. 
a is_parent d.    c is_parent i.     h is_parent n.     j is_parent s. 
b is_parent e.    d is_parent j.     i is_parent o.     m is_parent t. 
b is_parent f.    e is_parent k.     i is_parent p. 

%X and Y are siblings
:- op(500, xfx, 'is_sibling_of').

X is_sibling_of Y :-
  Z is_parent X,
  Z is_parent Y,
  X \== Y.

% X and Y are on the same level in the tree
:- op(500, xfx, 'is_same_level_as').

X is_same_level_as X.
X is_same_level_as Y :-
  W is_parent X,
  Z is_parent Y,
  W is_same_level_as Z.

% depth of node in the tree.
:- op(500, xfx, 'has_depth').
a has_depth 0 :- !.
Node has_depth D :-
  Mother is_parent Node,
  Mother has_depth D1,
  D is D1+1.

% locate node by finding a path from root down to the node.
locate(Node) :-
  path(Node),
  write(Node),
  nl.

path(a).
path(Node) :-
  Mother is_parent Node,
  path(Mother),
  write(Mother),
  write(' ---> ').

% calculate the height of a node, length of longest path to a leaf under the node

height(N, H) :-
  setof(Z, ht(N, Z), Set),
  max(Set, 0, H).

ht(Node, 0) :-
  leaf(Node),
  !.
ht(Node, H) :-
  Node is_parent Chid,
  ht(Child, H1),
  H is H1+1.

leaf(Node) :-
  not(is_parent(Node, Child)).

max([], M, M).
max([X|R], M, A) :-
  (X > M -> max(R, X, A); max(R, M, A)).

%member definition
olol(X, [X|_]).
olol(X, [_|R]) :- olol(X, R).

takeout(X, [X|R], R).
takeout(X, [F|R], [F|S]) :-
  takeout(X, R, S).

reverse2([], []).
reverse2([H|T], R) :-
  reverse2(T, RevT),
  append(RevT, [H], R).

perm([X|Y], Z) :-
  perm(Y, W),
  takeout(X, Z, W).
perm([], []).

delete(X, [X|T], T).
delete(X, [Y|T], [Y|T1]) :-
  delete(X, T, T1).

% don't understand ????
prune([], []).
prune([H|T], B) :-
  member(H, T),
  !,
  prune(T, B).
prune([H|T], [H|B]) :-
  prune(T, B).

prefix([], _).
prefix([H|PT], [H|T]) :-
  prefix(PT, T).

segment(_, []) :-
  false.
segment([H|ST], [H|T]) :-
  prefix(ST, T),
  !.
segment(S, [_|T]) :-
  segment(S, T).

%change for a dollar

change([H, Q, D, N, P]) :-
  member(H, [0, 1, 2]),
  member(Q, [0, 1, 2, 3, 4]),
  member(D, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]),
  member(N, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
            11, 12, 13, 14, 15, 16, 17, 18, 19, 20]),
  S is 50*H + 25*Q+10*D+5*N,
  S =< 100,
  P is 100-S.
