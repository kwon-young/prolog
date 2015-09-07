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
