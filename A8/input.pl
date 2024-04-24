append([ ], L, L).
append([X|R], L, [X|Z]) :- append(R, L, Z).

rev([], []).
rev([H|T], R) :- rev(T, R1), append(R1, [H|[]], R).

darth(vader).
food(lol).
meal(X) :- food(X).
study(_).
x(a(X), b(X), c(d(X)), e(Y)) :- fail.

a(X, Y) :- X = Y.
b(X, Y) :- X =/= Y.
g(X, Y) :- X > Y.
l(X, Y) :- X < Y.

edge(a, b).
edge(b, c).
edge(c, d).
edge(c,a).
edge(d, e).
path(X, Y) :- edge(X, Y).
path(X, Y) :- edge(X, Z), path(Z, Y).

r(X, ret(X)).
mem(_, []) :- fail.
mem(X, [X|_]).
mem(X, [_|T]) :- mem(X, T).

red(apple, ball).
hot(fire, ball).

abc(X, Y) :- red(X, Z), hot(Y, Z).