clear :-
  write('\33\[2J'),
  nl.

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
