subsetOfSize(0,_,[])        :- !.
subsetOfSize(K,[X|C],[X|S]) :- Kp is K-1, length(C,L), L >= Kp, subsetOfSize(Kp,C,S).
subsetOfSize(K,[_|C],S)     :- length(C,L), L >= K, subsetOfSize(K,C,S).

nutrientOfProducts(X,P) :- member(Pi,P), product(Pi,Npi), member(X,Npi).

shopping(K,L) :-
    findall(I, (numNutrients(NN), between(1,NN,I)), N), % Listamos todos los nutrientes en N
    findall(Pi, product(Pi, _), P), % Listamos todos los productos en P
    between(1,K,Nproducts), % Dado un número de productos Np entre 1 y K
    subsetOfSize(Nproducts,P,L), % Y un subconjunto de Np productos
    setof(X, nutrientOfProducts(X,L), N). % Queremos que su cjto global de nutrientes sea N
