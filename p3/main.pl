:- consult('A-caballo'),
   consult('A-haceraguas'),
   consult('B-puzzle'),
   consult('C-dieta'),
   consult('D-mainroads').

test(X) :- X, !, testRepeat(X).
test(X) :- write('no '), write(X), write('.'), nl.
testRepeat(X) :- X, write(X), write('.'), nl, fail.
testRepeat(_).

testW(X) :- write(X), write(': '), X, nl, !.
testW(_) :- write('no'), nl, !.

numNutrients(20).
product(milk, [1,2,3]).
product(salad, [5,7]).
product(orange, [8]).
product(strawberry, [1,2]).
product(bread, [5,4]).
product(cherry, [8,7,6]).
product(meal, [15,6]).
product(fish, [18,19,20]).
product(pasta, [15,2,1]).
product(tomato, [17,16,14]).
product(chicken, [13,6,7]).
product(spaguetti, [9,10]).
product(pizza, [11,5,6,2,10,15,20,19,18]).
product(cheese, [12]).

cities([1,2,3,4]).
road(1,2,10).
road(1,4,20).
road(2,3,25).
road(3,4,12).

main :-
    write('hacerAguas: '), hacerAguas, nl,
    testW(movimientosCaballo(10, [2,2], [3,5], 1)),
    testW(movimientosCaballo(10, [2,2], [3,4], 1)),
    testW(movimientosCaballo(2, [2,2], [3,4], 1)),
    testW(movimientosCaballo(5, [4,4], [1,1], 2)),
    testW(solve([[4,1],[4,2],[5,2],[5,3]])), !,
    testW(solve([[1,1],[2,1],[2,2],[3,2],[3,3],[4,1],[4,2],[4,3],[4,4],[5,1],[5,2],[5,3],[5,4],[5,5]])),
    test(shopping(8,_)),
    test(shopping(4,_)),
    test(shopping(7,_)),
    test(mainroads(42,_))
    .
