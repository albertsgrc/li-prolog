# Práctica 3 - Prolog

Hay un archivo para cada problema.

Los predicados correspondientes son:

- A-haceraguas: hacerAguas o bien solucionOptima, que escribe por pantalla
  la lista de estados de los cubos [LitrosCubo5, LitrosCubo8], por el orden
  en el que se alcanzan, desde el estado [0,0] hasta el estado [0,4].

- A-caballo: movimientosCaballo(N, Pi, Pf, P), que escribe el camino C, |C|=P,
  de posiciones del tablero [X,Y], 1 <= X <= N, 1 <= Y <= N por las
  que pasa el caballo, siendo la primera del camino Pi y la última Pf. Si no existe
  tal camino, el predicado falla.

- B-puzzle: solve(Dots), tal y como está definida en el enunciado.

- C-dieta: shopping(K,L), tal y como está definida en el enunciado.

- D-mainroads: mainroads(K,M), tal y como está definida en el enunciado.
