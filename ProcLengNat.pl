%%%%%%
% Definicion de Gramaticas (DCG): Concordandia Genero y Numero
% True: oracion([kevin, y, valery,comen,helado,en,la, cocina],[]).
% False: oracion([kevin, y, valery,comen,helado,hacia, cuchara],[]).


%Para hacer el analisis la oracion2 se va a encargar de responder la
%pregunta en base a la informacion contenida en el cuerpo de la texto
%a analizar, el agregar nuevo conocimiento se hace medianta clausulas
%que son agregadas por medio de asserts.
%sustantivo2(carro).
%
%
%Para generar el grafo:
%La construcion del grafo se realiza recursivamente, primero cada una
% de las hojas devuelve el elemento del grafo que logro reconocer
% (sustantivos,nombres propios, preposiciones o verbos)que logro
% reconocer, despues de en la parte de reconocimiento de los sintagmas
% agrega cada uno de los elementos al grafo y sus arcos con otros
% elementos, siempre hay que verificar que el elemento que se va a
% agregar no existe en el grafo antes de agregar cualquier arco con
% otro elemento.Los elementos son agregados la primera vez que se
% referencian en el texto y a partir de hay cualquier otra referencia
% que se haga a ese elemento en el corpus se tomara como que se
% referencia al mismo elemnto.
% Para responder preguntas se mapeara un la interogante a una red
% semantica, pero que a diferencia del texto base tendra una clausula
% con un nodo que se desconoce y representa ademas la respuesta a la
% pregunta.
% ejemplo:
% Quienes comen helado?
% arco(X,comen), arco(comen,helado).
%Nota: las proposiciones van siempre despues de
% los verbos,

% grafo([a,b,c,d,e,g,h],[arco(a,b), arco(a,d),
% arco(b,d),arco(d,g,)marco(e,h)],C).

:-[grafos].

oracion-->sintagmaNominal(G,N,H),sintagmaVerbal(G,N,H).  %G: genero y N: numero

%analisisProlog(X):-oracion(X,[]).
%analisisProlog():-oracion([kevin, y, valery,comen,helado,en,la,
%cocina],H), oracion2([a, y, valery,comen,helado,en,la, cocina],[]).
sintagmaNominal(G,N,H)--> (nombre_personal(G,E), {agregarAGrafo(E,H)});sustantivo(G,N,H).
sintagmaNominal(G,N,H)--> articulo(G,N),sustantivo(G,N,H).
sintagmaNominal(G,N,H)--> sustantivo(G,N,H), adjetivo(G,N,H).
sintagmaNominal(G,N,H)--> articulo(G,N), adjetivo(G,N,H),sustantivo(G,N,H).
sintagmaNominal(m,p,H)--> nombre_personal(_,H),conjuncion,sintagmaNominal(_,_,H).
sintagmaNominal(m,p,H)--> articulo(G,N),sustantivo(G,N,H),conjuncion,sintagmaNominal(_,_,H).

%oracion2-->sintagmaNominal2(G,N),sintagmaVerbal2(G,N).
% sintagmaNominal2(G,N)--> (articulo(G,N),sustantivo2(_));
% (nombrePersonal2(_),conjuncion,sintagmaNominal2(_,_)).


unifica(E2,E2):-write($E2).


%agregar(H,[]).
%agregar(H,E):-
%agregar(H|E, []).

articulo(f,s)--> [una],[la].
articulo(f,p)--> [las];[unas].
articulo(m,s)--> [el];[un].
articulo(m,p)--> [los];[unos].

sustantivo(m,s,E)--> [helado],{unifica(E, "helado")}.
sustantivo(m,s,E)--> [chocolate],{unifica(E, "chocolate")}.
sustantivo(m,s,E)--> [carro],{unifica(E, "carro")}.

sustantivo(m,p,E)--> [helados],{unifica(E,"helados")}.
sustantivo(m,p,E)--> [chocolates],{unifica(E,"chocolates")}.
sustantivo(m,p,E)--> [carros],{unifica(E,"carros")}.

sustantivo(f,s,E)--> [camisa],{unifica(E,"camisa")}.
sustantivo(f,s,E)--> [fresa],{unifica(E,"fresa")}.
sustantivo(f,s,E)--> [cuchara],{unifica(E,"cuchara")}.
sustantivo(f,s,E)--> [cocina],{unifica(E,"cocina")}.

sustantivo(f,p,E)--> [camisas],{unifica(E,"camisas")}.
sustantivo(f,p,E)--> [fresas],{unifica(E,"fresas")}.
sustantivo(f,p,E)--> [cucharas],{unifica(E,"cucharas")}.
sustantivo(f,p,E)--> [cocinas],{unifica(E,"cocinas")}.


nombre_personal(f,E)--> [ana];{unifica(E,"ana")}.
nombre_personal(f,E)--> [valery],{unifica(E,"valery")}.
nombre_personal(m,E)--> [carlos],{unifica(E,"carlos")}.
nombre_personal(m,E)--> [kevin],{unifica(E,"kevin")}.

adjetivo(m,s,E)--> [rico],{unifica(E,"rico")}.
adjetivo(m,p,E)--> [ricos],{unifica(E,"ricos")}.
adjetivo(f,s,E)--> [linda],{unifica(E,"linda")}.
adjetivo(f,p,E)--> [lindas],{unifica(E,"lindas")}.

%agregarAGrafoElemento(R,G).

conjuncion--> [y];[o].

%%
% Sintagma Verbal: Verbo + Predicado
% Conconrdandia de Verbo y Preposiciones
%%

sintagmaVerbal(G,N,H)-->(verboS(G,N,H));(verboE(G,N,H)).
sintagmaVerbal(G,N,H)-->(verboS(G,N,H),predicadoS(H));(verboE(G,N,H),predicadoE(H)).

verboS(_,s,E)--> [come];[tiene],{unifica(E,"come")}.
verboS(_,p,E)--> [comen];[tienen],{unifica(E,"comen")}.
verboE(_,s,E)--> [corre];[camina],{unifica(E,"corre")}.
verboE(_,p,E)--> [corren];[caminan],{unifica(E,"corren")}.

predicado_directo(H)--> sintagmaNominal(_,_,H).

predicadoS(H)--> predicado_directo(H).
predicadoS(H)--> predicado_indirectoS(H).
predicadoS(H)--> predicado_directo(H),predicado_indirectoS(H).
predicado_indirectoS(H)--> sintagmaNominal(_,_,H), preposicionS(H), sintagmaNominal(_,_,H).

predicadoE(H)--> predicado_indirectoE(H).
predicado_indirectoE(H)--> preposicionE(H), sintagmaNominal(_,_,H).

%Preposiciones Referencia
preposicionS(E)--> [de],{unifica(E,"de")}.
preposicionS(E)--> [con],{unifica(E,"con")}.
preposicionS(E)--> [en],{unifica(E,"en")}.

%Preposiciones Espaciales
preposicionE(E)--> [desde];[hacia];[en],{unifica(E,"desde")}.
preposicionE(E)--> [hacia],{unifica(E,"hacia")}.
preposicionE(E)--> [en],{unifica(E,"en")}.

%%%
%
% Ejemplos oraciones simples:
%
% ana y carlos comen
% ana y carlos comen un helado rico
%
% Ejemplos con preposiones de referencia:
%
% kevin y valery comen helado de fresa
% kevin y valery comen helado con cuchara
% carlos y ana tienen una camisa linda
%
% Ejemplos con preposiones de espaciales:
% kevin y valery corren en la cocina
% Kevin y ana corren desde la cocina
% carlos y valery caminan hacia los carros
%
%%%



















