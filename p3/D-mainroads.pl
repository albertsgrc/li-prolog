oneCity(C1,C2,C,C1) :- member(C2,C), !, \+member(C1,C).
oneCity(C1,C2,C,C2) :- member(C1,C), !.

mainroads(CS,_,_,C,M,M) :- msort(C, CS). % The sorted city lists must be equal
mainroads(CS,K,L,C,R,M) :-
    road(C1,C2,RLen), % Pick a road
    Lp is L + RLen, Lp =< K, % Ensure that new length is admissible
    oneCity(C1,C2,C,Nc), % Ensure that exactly one of both cities is in the tree, so we have no cycles
    mainroads(CS,K,Lp,[Nc|C],[road(C1,C2,RLen)|R], M). % Add the new city and the new road, recurse

mainroads(K,M) :- cities(C), msort(C, CS), member(S, C), !, mainroads(CS,K,0,[S],[],M).
