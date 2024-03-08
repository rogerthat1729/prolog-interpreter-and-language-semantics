/* Membership function for G */
mem((X, T), []) :- fail.
mem((X, T1), [(X, T2)|_]) :- T1\=T2, !, fail.
mem((X, T), [(X, T)|_]) :- !.
mem((X, T), [_|R]) :- mem((X, T), R), !.

/* G is a list of tuples, and one variable can be mapped to exactly one type, which has been handled */

/* Given functions */

hastype(G, myNum(N), intT) :- integer(N).
hastype(G, myBool(B), boolT) :- B = true; B = false.
hastype(G, myVar(X), T) :- mem((X, T), G).

hastype(G, add(E1, E2), intT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, mul(E1, E2), intT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, eq(E1, E2), boolT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, gt(E1, E2), boolT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, and(E1, E2), boolT) :- hastype(G, E1, boolT), hastype(G, E2, boolT).
hastype(G, or(E1, E2), boolT) :- hastype(G, E1, boolT), hastype(G, E2, boolT).
hastype(G, not(E), boolT) :- hastype(G, E, boolT).

/* New functions and types start here */

/* Basic integer functions */
hastype(G, subt(E1, E2), intT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, div(E1, E2), intT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, lt(E1, E2), boolT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, mod(E1, E2), intT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, abs(E), intT) :- hastype(G, E, intT).
hastype(G, neg(E), intT) :- hastype(G, E, intT).
hastype(G, pow(E1, E2), intT) :- hastype(G, E1, intT), hastype(G, E2, intT).

/* Basic boolean functions */
hastype(G, xor(E1, E2), boolT) :- hastype(G, E1, boolT), hastype(G, E2, boolT).
hastype(G, nand(E1, E2), boolT) :- hastype(G, E1, boolT), hastype(G, E2, boolT).
hastype(G, nor(E1, E2), boolT) :- hastype(G, E1, boolT), hastype(G, E2, boolT).

/* Combinatorical functions */
hastype(G, combination(E1, E2), intT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, permutation(E1, E2), intT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, factorial(E), intT) :- hastype(G, E, intT).

/* Ternary operator */
hastype(G, if(E1, E2, E3), T) :- hastype(G, E1, boolT), hastype(G, E2, T), hastype(G, E3, T).

/* Pair of expressions */
hastype(G, myPair(E1, E2), pairT(T1, T2)) :- hastype(G, E1, T1), hastype(G, E2, T2).
hastype(G, first(E), T1) :- hastype(G, E, pairT(T1, _)).
hastype(G, second(E), T2) :- hastype(G, E, pairT(_, T2)).

/* List of expressions */
hastype(G, emp, listT(_)) :- !.
hastype(G, cons(E, L), listT(T)) :- hastype(G, E, T), hastype(G, L, listT(T)).
hastype(G, pop(L), listT(T)) :- L \= emp, hastype(G, L, listT(T)).
hastype(G, size(L), intT) :- hastype(G, L, listT(_)).

/*
Testcases for given functions:
1. Number: hastype([], myNum(1), T). gives output intT
2. Boolean: hastype([], myBool(true), T). gives output boolT
3. Variable: hastype([(x, intT)], myVar(x), T). gives output intT
4. Addition: hastype([], add(myNum(1), myNum(2)), T). gives output intT
5. Equality: hastype([], eq(myNum(1), myNum(2)), T). gives output boolT
6. And: hastype([], and(myBool(true), myBool(false)), T). gives output boolT
7. Not: hastype([], not(myBool(true)), T). gives output boolT

Testcases for new functions:
1. Pair of expressions: hastype([], myPair(myNum(1), myBool(true)), T). gives output pairT(intT, boolT)
2. First of pair: hastype([], first(myPair(myNum(1), myBool(true))), T). gives output intT
3. Second of pair: hastype([], second(myPair(myNum(1), myBool(true))), T). gives output boolT
4. Cons of list: hastype([], cons(myNum(1), cons(myNum(2), emp)), T). gives output listT(intT)
5. Pop of list: hastype([], pop(cons(myBool(false), cons(myBool(true), emp))), T). gives output listT(boolT)
6. Size of list: hastype([], size(cons(myBool(false), cons(myBool(true), emp))), T). gives output intT
7. If-then-else: hastype([], if(eq(myNum(1), myNum(2)), myNum(1), myNum(2)), T). gives output intT

Crucial Testcases:
1. To test that one variable takes only one value: hastype(G, gt(mul(myNum(3), myVar(x)), myVar(x)), boolT) gives output false, as x cannot take two types.
Test hastype(G, gt(mul(myNum(3), myVar(x)), myVar(y)), boolT) gives output G = [(x, intT), (y, boolT)]
2. Testing variables in list: hastype(G, cons(myVar(x), cons(myNum(1), cons(myNum(2), emp))), T) gives G = [(x, intT)], T = listT(intT)
3. hastype(G, pop(cons(myVar(y), cons(myNum(5), myVar(x)))), T). gives output T=listT(intT), G=[(y, intT), (x, listT(intT))|_]
Testcases for other operations have not been written as they have the same type structure as given functions
*/