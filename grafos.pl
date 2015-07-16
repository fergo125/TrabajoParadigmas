%grafo([a,b,c,d,e,g,h],[arco(a,b), arco(a,d),
% arco(b,d),arco(d,g,)arco(e,h)]).
:-dynamic arco/2.
:-[utiles].
grafo([[],[]]).
concatenar([],L,L).
concatenar(E,L,R):-
	miembro(L,E),!,concatenar([],L|E,R).
agregarAGrafo(E,Hd|_):-
	concatenar(E,Hd,Hd).
agregarArcoGrafo(E1,E2):-
	assert(arco(E1,E2)).









