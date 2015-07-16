% ------------------------------------------------------------------------------
%
% UCR - Facultad de Ingeniería - ECCI
% CI-1441 Paradigmas Computacionales
% I-2015, Prof. Dr. Alvaro de la Ossa
%
% utiles.pl -- definiciones de ejemplos de predicado Prolog:
%
% a. De manejo/procesamiento de listas:
%
%    tamano/2(+L,-T): devuelve en T la cantidad de elementos de la lista L
%    nesimo/3(+N,+L,-O): devuelve en O el N-ésimo elemento de la lista L
%    miembro/2(+X,+L): devuelve 'true' si X es elemento de la lista L
%    maximo/2(+L,-M): devuelve en M el máximo elemento en la lista L
%    sumaLista/2(+L,-S): devuelve en S la suma de los elementos de la lista L
%    sumatoria/2(+L,-S): devuelve en S la expresión simbólica que representa
%    concatena/3(+L1,+L2,-C): la lista C es la concatenación de L1 y L2;
%       definición interativa (recursiva)
%    concatena2/3(+L1,+L2,-C): la lista C es la concatenación de L1 y L2;
%       definición más eficiente, basada en la unificación
%    reemplaza/4(+X,+Y,+L1,-L2): devuelve la lista L2 que resulta de
%       reemplazar en la lista L1 todas las ocurrencias de X por Y
%    reemplaza2/4(+X,+Y,+L1,-L2): versión simplificada de reemplaza/4 en la
%       que no se verifica que el objeto a reemplazar no sea una variable
%    reversa/2(+L,-R): R es la lista reversa de la lista L
%    rev/3(+L,?P,-R): auxiliar de reversa/2 que usa una pila P para invertir
%       la lista L
%
% b. De soluciones recursivas a problemas específicos (y sus auxiliares):
%
%    hanoi/4(+N,+Inicial,+Final,+Intermedia): resuelve las Torres de Hanoi
%    decir/2(+De,+A): auxiliar de hanoi/4 que muestra un movimiento
%    hanoi/1(+N): versión que usa sus propios nombres de torre; interfaz a
%       el predicado mover que es el que resuelve el juego
%    mover/4(+N,+De,+A,+Temp): resuelve las Torres de Hanoi
%    ancestro/2(+P1,+P2): determina si P2 es ancestro de P1
%              (+P1,-P2): encuentra los ancestros P2 de P1
%              (-P1,+P2): encuentra de quiénes es P2 ancestro
%              (-P1,-P2): muestra las relaciones de ancestro que se puedan
%                         encontrar en el programa Prolog
%
% c. De modificación de cláusulas del programa (adición y eliminación):
%    afirmaTodo/1(+L): afirma todas cláusulas contenidas en la lista L
%    retractaTodo/1(+P): retracta todas las cláusulas que unifiquen con el
%       patrón P; predicado de interfaz a retractalos/2, que hace el trabajo
%    retractalos/2: auxiliar de retractaTodo para retractar todas las cláusulas
%       que unifiquen con el patrón
%
% e. Otros:
%    negacion/2(+E,-V): devuelve en V el valor negado de E
%    max2/3(+X1,+X2,-M): devuelve en M el máximo entre X1 y X2
%    convierteTemp/2
%
% ------------------------------------------------------------------------------


% --- tamano/2(+L,-T) ----------------------------------------------------------
%     T es el tamaño de la lista L
%
% --- ejemplos: tamano([a,b,c],N).  -> N = 3

tamano([], 0).			% si no hay elementos, el tamaño es 0
tamano([_|R], N) :-		% en otro caso, calculamos el tamaño de la cola,
   tamano(R, M), N is M + 1.	% y el de la lista es uno más que ese


% --- nesimo/3(+N,+L,-X) -------------------------------------------------------
%     X es el N-ésimo elemento de la lista L
%
% --- ejemplo: nesimo(3,[a,b,c,d,e],X).  -> X = c

nesimo(1,[X|_],X).			% si se busca el primero, devuelve la cabeza;
nesimo(N,[_|R],E) :-			% en otro caso,reduce el contador en 1
   M is N - 1, nesimo(M,R,E).	% y recurre sobre la cola

% --- miembro/2(+X,+L)  --------------------------------------------------------
%     ¿es X miembro de L?
%
% --- ejemplo: miembro(c,[a,b,c,d,e]).  -> verdadero

miembro(X, [X|_]).			% si X es el elementos de la cabeza,
miembro(X, [_|R]) :-			% es miembro; en otro caso,
   miembro(X, R).			% lo buscamos en la cola


% --- max2/3(+X,+Y,-M) ---------------------------------------------------------
%     M es el máximo de X y Y
%
% --- nota: observe el uso del operador disyuntivo ';', en lugar de varias
%           cláusulas.

max2(X,Y,Max) :-
   X = Y, Max = Y;			% si son iguales, el máximo es cualquiera,
   X > Y, Max = X;			% si X es mayor, X es el máximo,
   Y > X, Max = Y.			% en otro caso, Y es el máximo


% --- maximo/2(+L,-M) ----------------------------------------------------------
%     M es el elemento máximo de la lista L

maximo([X],X).				% si solo hay un elemento, es el máximo
maximo([X|Xr],M) :-			% en otro caso,
   maximo(Xr,Y),			% el máximo debe ser el mayor entre
   ((X >= Y, M = X); M = Y), !.	% la cabeza y el máximo de la cola

% --- convierteTemp/2(?C,?F) ----------------------------------------------------------
%     C es la temperatura en Celsius, F en Fahrenheit
%     convierteTemp(+C,+F): determina si C y F sin equivalentes
%     convierteTemp(+C,-F): convierte de C a F
%     convierteTemp(-C,+F): convierte de F a C

convierteTemp(C,F) :-
   var(C), var(F), write('Error en los argumentos'), nl, !, fail.

convierteTemp(C,F) :-
   var(C), C is (F - 32) * 5 / 9.

convierteTemp(C,F) :-
   var(F), F is (C * 9 / 5) + 32.

convierteTemp2(C,F) :-
   var(C), C is (F - 32) * 5 / 9 ;
   var(F), F is (C * 9 / 5) + 32.

% --- sumaLista/2(+L,-S) -------------------------------------------------------
%     S es la suma de los elementos de la lista L

sumaLista([],0).			% si la lista está vacía, su suma es 0;
sumaLista([X|R],T) :-		% en otro caso, la suma de la lista es la suma
   sumaLista(R,T1), T is T1 + X.	% de la cola más el primer elemento


% --- sumatoria/2(+L,-S) ------------------------------------------------------
%     S es la sumatoria (la expresión simbólica) de los elementos de la lista L

sumatoria([],0).			% si la lista está vacía, su suma es 0;
sumatoria([X|R],T) :-		% en otro caso,añadimos a la sumatoria el
   sumatoria(R,T1), T = T1 + X.	% nuevo término


% --- concatena/3(+L1,+L2,-L3) – versión iterativa (recursiva) -----------------
%     L3 es la lista que resulta de concatenar L1 y L2

concatena([],L,L).			% concatenar una lista vacía y L es L;
concatena([X|Xr],Y,[X|Zr]) :-	% si no es vacía, la cabeza de concatenación
   concatena(Xr,Y,Zr).		% es la de la 1a lista y su cola es la concate-
						% nación de la cola de la primera y la segunda


% --- concatena2/3(+L1,+L2,-L3) – versión eficiente basada en la unificación ---
%     L3 es la lista que resulta de concatenar L1 y L2
%
% Se representa cada lista como una diferencia de dos listas: L = L1-L2.
% El truco es añadir y luego quitar a la lista un resto inexistente: la lista
% lista [a,b,c] se representa como [a,b,c|R]-R.
%
% Utilizando esta representación, la concatenación de dos listas X y Y debe
% ser X1-Y2, donde X=X1-X2 y Y=Y1-Y2.
%
% Ejemplo:
% A=[a,b,c]     se representa como A=[a,b,c|Ar]-Ar
% B=[d,e]       se representa como B=[d,e|Br]-Br
% C=[a,b,c,d,e] se representa como C=[a,b,c,d,e|Cr]-Cr
% Nótese que Cr resulta ser Br, si se concatena A-B con B-C, que debe ser A-C:

concatena2(X-Y,Y-Z,X-Z).	% la lista [a,b,..] se escribe como [a,b,..|R]-R

% Veamos cómo la concatenación de [a,b,c|Ar]-Ar con [d,e|Br]-Br] resulta ser
% la lista [a,b,c,d,e|Cr]-Cr:
%
% A = X-Y = [a,b,c|Ar] - Ar   --->  X = [a,b,c|Ar], Y = Ar
%                             --->  X = [a,b,c|Y]
% B = Y-Z = [d,e|Br]   - Br   --->  Y = [d,e|Br]  , Z = Br
%                             --->  Y = [d,e|Z]
%
% por lo tanto:
%
% C = X-Z = [a,b,c|Y]       - Z
%         = [a,b,c|[d,e|Z]] - Z
%         = [a,b,c,d,e|Z]   - Z  que representa la lista [a,b,c,d,e]


% --- reemplaza/4(+Viejo,+L1,+Nuevo,-L2) ---------------------------------------
%     L2 es la lista que resulta de reemplazar todas
%     las instancias de Viejo con Nuevo en la lista L1

reemplaza(_,[],_,[]).			% lista vacía: no hay nada que hacer
reemplaza(V,[X|R1],N,[N|R2]) :-		% si el objeto a reemplazar es la cabeza
   nonvar(X), X = V, !,			% se reemplaza
   reemplaza(V, R1, N, R2).		% y se recorre el resto de la lista
reemplaza(V,[X|R1],N,[X|R2]) :-		% en otro caso, se deja intacta,
   reemplaza(V,R1,N,R2).			% y se recorre el resto de la lista


% --- reemplaza2/4(+Viejo,+L1,+Nuevo,+L2) --------------------------------------
%     segunda versión de reemplaza

reemplaza2(_,[],_,[]).			% lista vacía: no hay nada que hacer
reemplaza2(V,[V|R1],N,[N|R2]) :-	% si el objeto a reemplazar es la cabeza
   reemplaza2(V,R1,N,R2), !.			% se reemplaza y se proceso el resto
reemplaza2(V,[Y|R1],N,[Y|R2]) :-	% en otro caso,
   reemplaza2(V,R1,N,R2), !.			% se deja intacto y se procesa el resto


% --- reversa/2(+Lista,-Reversa) -----------------------------------------------
%     Reversa es el resultado de invertir los elementos de la Lista

reversa(Lista,Reversa) :-			% usamos una pila (inicialmente vacía) para
   rev(Lista,[],Reversa).			% invertir la lista, y se la pasamos a rev/3


% --- rev/3() ------------------------------------------------------------------

rev([],Reversa,Reversa).			% si la lista está vacío, se devuelve vacío;
							% en otro caso se pone la cabeza de la lista
rev([X|ColaX],Pila,Reversa) :-		% en la pila y se procesa el resto, y se
   rev(ColaX,[X|Pila],Reversa).		% devuelve a reversa/2 la pila


% --- hanoi/4(+N,+De,+A,+Usando) -----------------------------------------------
%     las torres de Hanoi

hanoi(0,_,_,_) :- !.					% (el usuario etiqueta las torres)
							% si no hay discos que mover, no hacer nada
hanoi(N,Origen,Destino,Temp) :-		% para pasar N discos del Origen al Destino,
   M is N-1,					% se resuelve el problema para N-1 discos:
   hanoi(M,Origen,Temp,Destino),	% se pasan los discos del Origen al Destino,
   decir(Origen,Destino),			% se muestra el movimiento, y
   hanoi(M,Temp,Destino,Origen).	% se pasa por último el N-ésimo disco

decir(De,A) :-					% este predicado
   write('* '), write(De),		% muestra de dónde
   write(' -> '), write(A), nl.		% hacia dónde se mueve un disco


% --- hanoi2/1(+N) -------------------------------------------------------------
%     otra versinn de las torres de Hanoi

hanoi(N) :-					% (etiquetamos las torres: izq,centro,der)
   mover(N,izq,centro,der).		% y mover se hacer cargo de resolverlo

mover(0,_,_,_) :- !.				% si no hay discos que mover, no hacer nada

mover(N,A,B,C) :-				% para pasar N discos de A a B,
   M is N-1,					% se resuelve el problema para N-1 discos:
   mover(M,A,C,B),				% se pasan los discos de A a B,
   decir(A,B),					% se muestra el movimiento, y
   mover(M,C,B,A).				% se pasa por último el N-ésimo disco


% --- ancestro(?X,?Y) –---------------------------------------------------------
%     +X,+Y: determina si Y es ancestro de X
%     +X,-Y: encuentra los ancestros de X
%     -X,+Y: encuentra de quiénes es ancestro Y

ancestro(X,Y) :-				% el padre y la madre de X son sus ancestros
   padre(X,Y) ; madre(X,Y).
ancestro(X,Y) :-				% y también lo son los ancestros de ellos
   ancestro(X,Z), ancestro(Z,Y).


% --- afirmaTodo/1(+ListaDeCláusulas) ------------------------------------------
%     afirma todos los hechos y reglas de la lista (sin revisión de sintaxis)

afirmaTodo([]).					% nada más que afirmar
afirmaTodo([C|Rc]) :- X = C,		% guardar el patrón C en X temporalmente
   afirmalo(X), afirmaTodo(Rc).		% afirmar X y recurrir sobre la cola
afirmalo(X) :- assert(X).


% --- retractaTodo/1(+Termino) -------------------------------------------------
%     retracta todos los hechos que se unifiquen con el término T

retractaTodo(T) :-				% para retractar todas las cláusulas que
   X = T, retractalos(T,X).		% unifican con T, primero "guardamos" una
							% copia del patrón en X
retractalos(T,X) :-				% luego retractamos c/una de las cláusulas
   retract(T), retractaTodo(X).		% en la base que unifican con T, y usamos X
retractalos(_) :- !.				% para "refrescar" el patrón; una vez todas
							% han sido retractadas, fallar


% --- El operador de corte (!, "cut")
%     Se utiliza para evitar la retroacción a una zona específica del árbol Y/O
%     de búsqueda del programa Prolog
%
% --- ejemplo (la negación como falla):

negacion(M,fail) :- M, !.			% si la meta M es verdadera, falla
negacion(_,true).				% en otro caso (es falsa), sale con éxito


% ------------------------------------------------------------------------------
%
% --- Problemas/Ejercicios:
%
% --- 1. Combinando los diseños de los predicados 'nesimo' y 'miembro', programe
%        un predicado 'posicion/3(+O,+L,-P)' que devuelva la posición P de la
%        primera instancia del objeto O en la lista L. Nota: este predicado
%        sería equivalente al uso de nesimo así: nesimo(-P,+L,+O), donde el
%        predicado devolvería en P la posición del objeto O en la lista L
%
% >>>
%
%        Nota: la definición que usted debe elaborar, si se fuerza al
%        backtracking, debe devolver todas soluciones al problema, es decir, las
%        posiciones de todas las instancias del objeto que se encuentren en la
%        lista
%
% --- 2. En forma similar al problema anterior, programe un predicado
%        subarbol/3(+O,+A,-S) que devuelva la sublista de S en la que se
%        encuentre la primera instancia del objeto O en la lista A
%
% >>>
%
% --- 3. Explique qué hace el intérprete Prolog con la meta
%        miembro(X,[a,b,c,d,e]).
%
% >>>
%
% --- 4. Reescriba el predicado 'max2/3' sin utilizar el operador disyuntivo ';'
%
% >>>
%
% --- 5. En el predicado 'maximo/2', ¿dónde podríamos incluir un corte para
%        evitar múltiples soluciones?
%
% >>>
%
% --- 6. Modifique el predicado 'negacion/2' eliminando el operador de corte,
%        y explique qué sucede
%
% >>>
%
% --- 7. Añada aquí hechos de su familia para probar el predicado 'ancestro/2'
%
%     Ejemplo:
%
madre(juan, ana).
madre(celia, maria).
madre(eduardo, celia).
madre(carlos, carla).
madre(teodoro, carla).
madre(isabel, sara).
madre(ximena, laura).
padre(juan, pedro).
padre(celia, jose).
padre(eduardo, juan).
padre(carlos, eduardo).
padre(teodoro, eduardo).
padre(isabel, eduardo).
padre(sara, andres).
%
% ?- ancestro(eduardo,pedro)   % ¿es pedro ancestro de eduardo?
% ?- ancestro(juan, X)         % ¿quiénes son ancestros de juan?
% ?- ancestro(X, laura)        % ¿de quiénes es laura ancestro?
%
% >>>
%
% --- 8. Añada el predicado siguiente a su programa Prolog usando afirmaTodo/1
%
% sub(X,[X|Rx],[X|Rx]).
% sub(X,[_|R],Z) :- sub(X,R,Z)]).
%
% >>>
%
%        Explique qué hace sub/3
%
% --- 9. Use retractaTodo/1 para eliminar todos los hechos de sub/3
%
% ?- retractaTodo(sub(_,_,_)).
%
%        Modifique retractaTodo/1 para que elimine también todas las reglas
%
% >>>
%
% --- 10. Utilice el predicado negacion/2 para verificar que en efecto invierte
%         el valor de retorno (true, fail/false) de la ejecución de una meta
%
% >>>
%
% ------------------------------------------------------------------------------
