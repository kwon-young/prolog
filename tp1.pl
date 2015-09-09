clear :-
  write('\33\[2J'),
  nl.

% for debug
% guitracer
% gtrace

%fait
sur(a, b).
sur(b, table).

%regle
auDessus(X, Z) :-
  sur(X, Z).
auDessus(X, Z) :-
  auDessus(X, Y), auDessus(Y, Z).

mot(abalone, a, b, a, l, o, n, e).
mot(abandon, a, b, a, n, d, o, n).
mot(enhance, e, n, h, a, n, c, e).
mot(anagram, a, n, a, g, r, a, m).
mot(connect, c, o, n, n, e, c, t).
mot(elegant, e, l, e, g, a, n, t).

puzzle(V1, V2, V3, H1, H2, H3):-
  mot(V1, _, A, _, B, _, C, _),
  mot(V2, _, D, _, E, _, F, _),
  mot(V3, _, G, _, H, _, I, _),
  mot(H1, _, A, _, D, _, G, _),
  mot(H2, _, B, _, E, _, H, _),
  mot(H3, _, C, _, F, _, I, _).

%nth(N, T, L) : T = LN
nth(1, T, [T|_]).
nth(N, X, [_|Q]):-
  nth(N1, X, Q),
  N is N1 + 1.

% in(T, L) : T E L
in(T, [T|_]).
in(T, [_|Q]) :-
  in(T, Q).

% conc(L1, L2) : L = L1 + L2
conc([], L, L).
conc([T|Q], L, [T|QL]) :-
  conc(Q, L, QL).

%reseau routier
route(s, a).
route(s, d).
route(a, b).
route(a, d).
route(b, c).
route(b, e).
route(d, e).

voisines(X, Y):-
  route(X, Y);
  route(Y, X).

%coloriage

adjacent(X, Y, Map) :-
  member([X, Y], Map);
  member([Y, X], Map).

%find_regions([[1,2],[1,3],[1,4],[1,5],[2,3],[2,4],[3,4],[4,5]],[],R).

find_regions([],R,R). 
find_regions([[X,Y]|S], R,A) :- 
     (member(X,R) ->  
        (member(Y,R) -> find_regions(S,R,A) ; find_regions(S,[Y|R],A)) ; 
           (member(Y,R) -> find_regions(S,[X|R],A) ; find_regions(S,[X,Y|R],A) ) ).

color([[1,2],[1,3],[1,4],[1,5],[2,3],[2,4],[3,4],[4,5]], 
      [red,green,blue,yellow],Coloring).
%Coloring = [[5,red],[4,green],[3,red],[1,blue],[2,yellow]] ; ...   [> 48 solutions <] 

color_all([H|T], Colors, [[H, C]|A]) :-
  member(C, Colors),
  color_all(T, Colors, A).
color_all([], _, []).

color(Map, Colors, Result) :-
  find_regions(Map, [], Regions),
  color_all(Regions, Colors, Result),
  \+ conflict(Map, Result).

takeout(X, [X|R], R).
takeout(X, [F|R], [F|S]) :-
  takeout(X, R, S).

conflict(_, []).
conflict(Map, Result) :-
  member([R1, C], Result),
  member([R2, C], Result),
  adjacent(R1, R2, Map),
  !.

