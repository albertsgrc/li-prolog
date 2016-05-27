writeSteps([]).
writeSteps([P-J|S]) :- write(P), write(' jumps over '), write(J), nl, writeSteps(S).

sumPair([X1,X2],[Y1,Y2],[R1,R2]) :- R1 is X1+Y1, R2 is X2+Y2.
halfPair([X,Y],[RX,RY]) :- RX is X>>1, RY is Y>>1.

betweenchk(X, L, R) :- L =< X, X =< R.
insideTriangle([X,Y]) :- X >= Y, betweenchk(X, 1, 5), betweenchk(Y, 1, 5).

puzzle([_], _).
puzzle(Dots, [Dot1-Dot2|R]) :-
    select(Dot1, Dots, DotsP), % Pick a dot and remove it from the list of dots
    member(Jump, [[0,2],[0,-2],[2,0],[-2,0],[2,2],[-2,-2]]), % Pick a jump

    sumPair(Dot1, Jump, JumpPos), % Compute position where we jump
    insideTriangle(JumpPos), % Check that the position where we jump is not out of bounds
    \+member(JumpPos, DotsP), % Check that the position where we jump is free

    halfPair(Jump, JumpHalf),
    sumPair(Dot1, JumpHalf, Dot2), % Compute the dot that we're jumping over
    selectchk(Dot2, DotsP, DotsPP), % Check that the dot we're jumping exists, and remove it

    puzzle([JumpPos|DotsPP], R). % Add the dot that jumped with its new position and recurse

solve(Dots) :-
    puzzle(Dots, S),
    writeSteps(S).
