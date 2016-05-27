% helpers
test(CALL) :- CALL, !, testRepeat(CALL).
test(CALL) :- write('NO '), write(CALL), write('.'), nl.

testRepeat(CALL) :- CALL, write(CALL), write('.'), nl, fail.
testRepeat(_).

% ex 2
prod([], 1).
prod([X|L], P) :- prod(L, P1), P is P1*X.

% ex 3
pescalar([], [], 0).
pescalar([X|L1], [Y|L2], P) :- pescalar(L1,L2,P2), P is P2 + X*Y.

% ex 4
in(X, [X|_]).
in(X, [_|L]) :- in(X,L).

not(P) :- P, !, fail.
not(_).

not_in(X, Y) :- not(in(X, Y)).

intersection([], _, []).
intersection([X|L1], L2, [X|R]) :- in(X,L2), !, intersection(L1,L2,R).
intersection([X|L1], L2, R) :- not_in(X,L2), intersection(L1,L2,R).

union([], X, X).
union([X|L1], L2, R) :- in(X,L2), !, union(L1,L2,R).
union([X|L1], L2, [X|R]) :- not_in(X,L2), union(L1,L2,R).

% ex 5
concat([], L2, L2).
concat([X|L1], L2, [X|R]) :- concat(L1,L2,R).

last(L, X) :- concat(_,[X],L).

inverse([], []).
inverse(L, [X|R]) :- concat(L2,[X],L), inverse(L2, R).

% ex 6
fib(1,1).
fib(2,1).
fib(N,F) :-
    N > 2,
    N1 is N-1, N2 is N-2,
    fib(N1,F1), fib(N2,F2),
    F is F1+F2.

% ex 7
dados(0,0,[]).
dados(P,N,[X|L]) :-
    N > 0,
    in(X, [1,2,3,4,5,6]),
    Q is P-X,
    M is N-1,
    dados(Q,M,L).

% ex 8
in_rest(X, L, R) :- concat(L1, [X|L2], L), concat(L1, L2, R).

sum([], 0).
sum([X|L], R) :- sum(L, R1), R is R1 + X.

suma_demas(L) :- in_rest(X, L, R), sum(R, X), !.

% ex 9
suma_ants(L) :- concat(L1, [X|_], L), sum(L1, X), !.

% ex 10
card([], []).
card([X|L], [[X,S]|R]) :- card(L, LR), in_rest([X, SP], LR, R), !, S is SP + 1.
card([X|L], [[X,1]|R]) :- card(L, R).

card(L) :- card(L, R), write(R).

% ex 11
esta_ordenada([]). % Will never be called recursively
esta_ordenada([_]) :- !. % Can be called recursively, so we need to avoid the generic alternative
esta_ordenada([X,Y|R]) :- X =< Y, esta_ordenada([Y|R]).

% ex 12
permutacion([], []).
permutacion(L, [X|L2]) :- in_rest(X, L, R), permutacion(R, L2).

ordenacion(L1, L2) :- permutacion(L1, L2), esta_ordenada(L2), !. % Si hi ha dos elements repetits pot generar més d'una permutació igual

% ex 13
% Coste en caso peor: O(n^2*(n-1)!) = O(n*n!)

% ex 14
insercion(X, [], [X]).
insercion(X, [Y|L], [X,Y|L]) :- X =< Y, !.
insercion(X, [Y|L], [Y|R]) :- insercion(X, L, R).

ordenacion_ins([], []).
ordenacion_ins([X|L], R) :- ordenacion_ins(L, R2), insercion(X, R2, R).

% ex 15
% Coste en caso peor: O(n^2)

% ex 16
split([], [], []).
split([A], [A], []).
split([A,B|S], [A|L], [B|R]) :- split(S, L, R).

merge([], LR, LR).
merge(LL, [], LL).
merge([X|LL], [Y|LR], [X|L]) :- X =< Y, !, merge(LL, [Y|LR], L).
merge([X|LL], [Y|LR], [Y|L]) :- merge([X|LL], LR, L).

ordenacion_merge([], []) :- !.
ordenacion_merge([X], [X]) :- !.
ordenacion_merge(L1, R) :-
    split(L1, LE, LR),
    ordenacion_merge(LE, LEO),
    ordenacion_merge(LR, LRO),
    merge(LEO, LRO, R).


% ex 17

nmembers(_, 0, []) :- !.
nmembers(L, N, [Y|R]) :- in(Y, L),  N1 is N - 1, nmembers(L, N1, R).

writeAll([]) :- !.
writeAll([X|L]) :- write(X), writeAll(L).

diccionario(A, N) :- nmembers(A, N, M), writeAll(M), write(' '), fail.
diccionario(_, _) :- nl.

% ex 18
es_palindromo(P) :- inverse(P, P).

palindromos(L) :- permutacion(L, P), es_palindromo(P), write(P), write(' '), fail.
palindromos(_) :- nl.

% ex 19
suml([], [], [], C, C).
suml([X1|L1], [X2|L2], [X3|L3], Cin, Cout) :-
    X3 is (X1 + X2 + Cin) mod 10,
    C  is (X1 + X2 + Cin) //  10,
    suml(L1,L2,L3,C,Cout).

send_more_money :-
    L = [S,E,N,D,M,O,R,Y,_,_],
    permutacion([0,1,2,3,4,5,6,7,8,9], L),
    suml([D,N,E,S], [E,R,O,M], [Y,E,N,O], 0, M),

    write('S = '), write(S), nl,
    write('E = '), write(E), nl,
    write('N = '), write(N), nl,
    write('D = '), write(D), nl,
    write('M = '), write(M), nl,
    write('O = '), write(O), nl,
    write('R = '), write(R), nl,
    write('Y = '), write(Y), nl,
    write('  '), write([S,E,N,D]), nl,
    write('  '), write([M,O,R,E]), nl,
    write('-------------------'), nl,
    write([M,O,N,E,Y]), nl.

% ex 20

simplifica(E,E1):- unpaso(E,E2),!, simplifica(E2,E1).
simplifica(E,E).

unpaso(A+B,A+C):- unpaso(B,C),!.
unpaso(B+A,C+A):- unpaso(B,C),!.
unpaso(A*B,A*C):- unpaso(B,C),!.
unpaso(B*A,C*A):- unpaso(B,C),!.
unpaso(0*_,0):-!.
unpaso(_*0,0):-!.
unpaso(1*X,X):-!.
unpaso(X*1,X):-!.
unpaso(0+X,X):-!.
unpaso(X+0,X):-!.
unpaso(N1+N2,N3):- number(N1), number(N2), N3 is N1+N2,!.
unpaso(N1*N2,N3):- number(N1), number(N2), N3 is N1*N2,!.
unpaso(N1*X+N2*X,N3*X):- number(N1), number(N2), N3 is N1+N2,!.
unpaso(N1*X+X*N2,N3*X):- number(N1), number(N2), N3 is N1+N2,!.
unpaso(X*N1+N2*X,N3*X):- number(N1), number(N2), N3 is N1+N2,!.
unpaso(X*N1+X*N2,N3*X):- number(N1), number(N2), N3 is N1+N2,!.

% ex 21

nmembersrest(L, 0, [], L) :- !.
nmembersrest(L, N, [Y|R], O) :- N1 is N - 1, nmembersrest(L, N1, R, O1), in_rest(Y, O1, O).

times([], _, 0).
times([E|L], E, N) :- times(L, E, N1), !, N is N1 + 1.
times([_|L], E, N) :- times(L, E, N).

misioneros_canibales_imm([], _, []) :- !.
misioneros_canibales_imm(O1, O2, [M|R]) :-
    in(CAPACITY, [1,2]),
    nmembersrest(O1, CAPACITY, M, O1p),
    concat(O2, M, O2p),

    times(O1p, c, C1), times(O1p, m, M1), M1 >= C1,
    times(O2p, c, C2), times(O2p, m, M2), M2 >= C2,

    misioneros_canibales_imm(O1p, O2p, R).

writeMoves([]) :- !.
writeMoves([M|O]) :- writeMoves(O), write(M), write(' ').

misioneros_canibales :- misioneros_canibales_imm([m,m,m,c,c,c], [], R), writeMoves(R).

main :-
    test(prod([2,3,3], _)),
    test(prod([], _)),
    test(prod([5,5,-1], _)),
    test(pescalar([2,5,3], [2,10,3], _)),
    test(intersection([2,5,10], [2,5,3], _)),
    test(union([2,5,10], [3,5,2], _)),
    test(concat([1,2,3], [4,5,6,7,8], _)),
    test(concat([1,2,3,4,5,6], [7,8,9], _)),
    test(last([1,2,3,4,5,6,7], _)),
    test(inverse([1,2,3,4,5,6], _)),
    test(fib(15, _)),
    test(dados(10,2, _)),
    test(suma_demas([2,1,5,22,3,11])),
    test(suma_demas([5,5,5,5,5])),
    test(suma_ants([1,2,3,4,5,6])),
    test(suma_ants([5,4,3,2,1,0])),
    test(card([2,3,4,4,4,5,1,0,1,5,2], _)),
    test(esta_ordenada([3,45,67,83])),
    test(esta_ordenada([3,67,45])),
    test(esta_ordenada([])),
    test(ordenacion([5,2,7,10,1], _)),
    test(ordenacion([], _)),
    test(ordenacion_ins([5,2,7,10,1,-1,199,22,-20,5,2,3,2,1,2,6,22,33], _)),
    test(ordenacion_ins([], _)),
    test(ordenacion_merge([5,2,7,10,1,-1,199,22,-20,5,2,3,2,1,2,6,22,33], _)),
    test(ordenacion_merge([], _)),
    write('diccionario([ga, chu, le], 3): '), diccionario([ga,chu,le], 3),
    write('palindromos([a,a,c,c]): '), palindromos([a,a,c,c]),
    write('send_more_money.'), nl, send_more_money, !,
    test(nmembersrest([2,3,3], 2, _, _)),
    write('misioneros_canibales.'), nl, misioneros_canibales,
    halt.
