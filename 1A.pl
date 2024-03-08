/* Part 1A */

:- use_module(library(lists)).

mem(X, []) :- fail.
mem(X, [X|_]).
mem(X, [_|R]) :- mem(X, R).

notequal((X, Y), (X1, Y1)) :- X=\=X1.
notequal((X, Y), (X1, Y1)) :- Y=\=Y1.

size([], 0) :- !.
size([_|L], N) :- size(L, N1), N is N1+1.

append([ ], L, L).
append([X|R], L, [X|Z]) :- append(R, L, Z).

mem((X, Y), rel_comp(S, N)) :- N=:=1, mem((X, Y), S), !.
mem((X, Y), rel_comp(S, N)) :- N=\=1, M is N-1, mem((Z, Y), S), mem((X, Z), rel_comp(S, M)).

mem((X, X), union_of_compositions(S, El, N)) :- N=:=0, mem(X, El), !.
mem((X, Y), union_of_compositions(S, El, N)) :- N>=1, mem((X, Y), rel_comp(S, N)), !.
mem((X, Y), union_of_compositions(S, El, N)) :- N>=1, M is N-1, mem((X, Y), union_of_compositions(S, El, M)).

mem((X, Y), rtclos(S, El)) :- size(S, N), mem((X, Y), union_of_compositions(S, El, N)).

inv([], []) :- !.
inv([(X, X)|S], S1) :- inv(S, S1).
inv([(X, Y)|S], [(Y, X)|S1]) :- X\=Y, inv(S, S1).

sym_clos(S, S1) :- inv(S, S2), append(S, S2, S1).

mem((X, Y), rstclos(S, El)) :- sym_clos(S, S1),!, mem((X, Y), rtclos(S1, El)).

/*
Testcases for reflexive-transitive closure:
1. mem((1, 3), rtclos([(1, 2), (2, 3)], [1, 2, 3])) gives true, as (1, 3) is present due to transitive property.
2. mem((2, 2), rtclos([(1, 2), (2, 3)], [1, 2, 3])) gives true, as (2, 2) is present due to reflexive property.
3. mem((1, 5), rtclos([(1, 2), (2, 3), (5, 4), (2, 1), (3, 4), (4, 5)], [1, 2, 3, 4, 5])) gives true, as there exits a path from 1 to 5, which can be achieved by the transitive property.
4. mem((1, 3), rtclos([(1, 1), (2, 3)], [1, 2, 3, 4, 5])) gives false, as (1, 3) cannot be achieved using transitive property.

Testcases for reflexive-symmetric-transitive closure:
1. mem((1, 5), rstclos([(1, 4), (4, 2), (3, 5)], [1, 2, 3, 4, 5])) gives false, which is correct as (1, 5) is not present in the set after closure.
2. mem((4, 5), rstclos([(4, 7), (5, 7)], [4, 5, 7])) gives true, as (4, 5) can be achieved by applying transitive property on set after symmetric closure, which contains (4, 7) and (7, 5).
3. mem((3, 3), rstclos([(1, 2), (2, 3), (5, 4)], [1, 2, 3, 4, 5])) gives true, as (3, 3) is achieved during reflexive closure.
*/



/* Part 1B */

/*
Testcases for unionI:
1. One of the sets is empty: unionI([], [1,2,3], X) -> gives X = [1, 2, 3] which is correct.
2. Both sets are same with different ordering: unionI([3,2,1], [1,2,3], X) -> gives X = [3, 2, 1], which is correct.
3. Both sets have different elements: unionI([1,2,3], [4,5,6], X) -> gives X = [1, 2, 3, 4, 5, 6], which is correct.
4. Some elements are repeated: unionI([2,3,4,1], [1,2,3], X) -> gives X = [2, 3, 4, 1], which is correct.

Testcases for powerI:
1. Empty set: powerI([], X) -> gives X = [[]], which is correct.
2. Non-empty set: powerI([1, 2, 3], X) -> gives X = [[1, 2, 3], [1, 2], [1, 3], [1], [2, 3], [2], [3], []], which is correct.
3. Set of strings/numbers: powerI([2, "ab", "cde"], X) -> gives X = [[2, "ab", "cde"], [2, "ab"], [2, "cde"], [2], ["ab", "cde"], ["ab"], ["cde"], []], which is correct.
*/

subset([], S) :- !.
subset([X|R], S) :- mem(X, S), subset(R, S).

eqset(S1, S2) :- subset(S1, S2), !, subset(S2, S1).

del(X, [ ] , [ ]) :- !.
del(X, [X|R], Z) :- del(X, R, Z).
del(X, [Y|R], [Y|Z]) :- X\=Y, del(X, R, Z).

remdups([ ], [ ]) :- !.
remdups([X|R], [X|Z]) :- del(X, R, L), remdups(L, Z).

mapcons(X, [ ], [ ]) :- !.
mapcons(X, [Y|R], [ [X|Y] | Z ]) :- mapcons(X, R, Z).

formtuples(X, [], []) :- !.
formtuples(X, [Y|R], [(X, Y)|Z]) :- formtuples(X, R, Z).

unionI([ ], S2, S2) :- !.
unionI(S1, [ ], S1) :- !.
unionI([X|R], S2, [X|Z]) :- del(X, S2, S3), unionI(R, S3, Z).

/* To check if union does not have duplicates: */

checkdup(S) :- remdups(S, S1), size(S, N1), size(S1, N2), N1=\=N2.
checkuniondup(S1, S2) :- unionI(S1, S2, S3), !, checkdup(S).

/*
checkdup([1, 2, 1, 3]) will return true as the set S contains duplicates, while checkdup([1, 2, 3]) will return false as it does not contain duplicates.
checkuniondup takes two sets S1, S2 and return true if their union has duplicates.
So, since unionI implementation is correct and results in no duplicates, checkuniondup([1, 2], [2, 3]) returns false.
*/

/* Intersection of two sets: */

interI([], S, []) :- !.
interI([X|S1], S2, S3) :- mem(X, S2), interI(S1, S2, R1), append([X], R1, S3).
interI([X|S1], S2, S3) :- \+mem(X, S2), interI(S1, S2, S3).

/* Testcases - 
1. Two sets with common elements: interI([1, 2, 3], [2, 3, 4], X) -> gives X = [2, 3], which is correct.
2. Sets with strings/numbers: interI([a, b, 1], [b, 1, c], X) -> gives X = [b, 1], which is correct.
3. Sets with no common elements: interI([4, 5, 6], [1, 2, 3], X) -> gives X = [], which is correct.
4. One of the sets is empty: interI([], [1, 2], X) -> gives X = [], which is correct.
*/

/* Difference of two sets: */

diffI([], S, []) :- !.
diffI([X|S1], S2, S3) :- \+mem(X, S2), diffI(S1, S2, R1), append([X], R1, S3).
diffI([X|S1], S2, S3) :- mem(X, S2), diffI(S1, S2, S3).

/* Testcases -
1. Two sets with common elements: diffI([1, 2, 3], [2, 3, 4], X) -> gives X = [1], which is correct.
2. Sets with strings/numbers: diffI([a, b, 1], [b, 1, c], X) -> gives X = [a], which is correct.
3. Sets with no common elements: diffI([4, 5, 6], [1, 2, 3], X) -> gives X = [], which is correct.
4. One of the sets is empty: diffI([1, 2], [], X) -> gives X = [1, 2], which is correct.
*/

/* Cartesian Product of two sets: */

cartesianI([], S, []) :- !.
cartesianI([X|S1], S2, S3) :- cartesianI(S1, S2, R1), formtuples(X, S2, R2), append(R2, R1, S3). 

/* Testcases -
1. One of the sets is empty: cartesianI([], [3, 4], X) -> gives X = [], which is correct.
2. Both sets are non-empty: cartesianI([2, 3], [3, 4], X) -> gives X = [(2, 3), (2, 4), (3, 3), (3, 4)], which is correct, and has 2*2 = 4 elements.
3. Sets with strings/numbers: cartesianI([2, b], [3, c, e], X) -> gives X = [(2, 3), (2, c), (2, e), (b, 3), (b, c), (b, e)], which is correct, with 2*3 = 6 elements.
*/

powerI([ ], [ [ ] ]) :- !.
powerI([X|R], P) :- powerI(R, P1),  mapcons(X, P1, P2), append(P2, P1, P).

union_of_powerset([], []) :- !.
union_of_powerset([X|R], S) :- union_of_powerset(R, R1), !, unionI(X, R1, S).

check_if_powersets_equal(S1, S2) :- powerI(S1, P1), powerI(S2, P2), union_of_powerset(P1, M1), union_of_powerset(P2, M2), eqset(M1, M2).

/* To check if powersets generated by two set representations in different order - I took the union of all sets in the powerset, and checked if this union is equal for two sets.
   This is a foolproof method as - the powersets generated for each of the sets are indeed powersets due to correctness of powerI, and therefore union of the elements within will give the original set. */