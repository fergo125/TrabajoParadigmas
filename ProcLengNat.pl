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

oracion-->sintagmaNominal(G,N,H),sintagmaVerbal(G,N,H).  %G: genero y N: numero

%analisisProlog(X):-oracion(X,[]).
% analisisProlog():-oracion([kevin, y, valery,comen,helado,en,la,
% cocina],H), oracion2([a, y, valery,comen,helado,en,la, cocina],[]).
sintagmaNominal(G,N,H)--> nombre_personal(G,H);sustantivo(G,N,H).
sintagmaNominal(G,N,H)--> articulo(G,N,H),sustantivo(G,N,H).
sintagmaNominal(G,N,H)--> sustantivo(G,N,H), adjetivo(G,N,H).
sintagmaNominal(G,N,H)--> articulo(G,N,H), adjetivo(G,N,H),sustantivo(G,N,H).
sintagmaNominal(m,p,H)--> nombre_personal(_,H),conjuncion,sintagmaNominal(_,_,H).
sintagmaNominal(m,p,H)--> articulo(G,N,H),sustantivo(G,N,H),conjuncion,sintagmaNominal(_,_,H).

%oracion2-->sintagmaNominal2(G,N),sintagmaVerbal2(G,N).
% sintagmaNominal2(G,N)--> (articulo(G,N),sustantivo2(_));
% (nombrePersonal2(_),conjuncion,sintagmaNominal2(_,_)).

:-dynamic sustantivo2/1.
:-dynamic nombrePersonal2/1.
:-dynamic verboS2/1.
:-dynamic verboE2/1.

:-dynamic elemento/1.
:-dynamic elemento/2.


articulo(f,s,H)--> [una],{assert(elemento(H))}.
%agregar(H,[]).
%agregar(H,E):-
%agregar(H|E, []).

articulo(f,s,E)--> [la],{ E is  "la"}.
articulo(f,p,H)--> [las];[unas],{assert(elemento(H))}.
articulo(m,s,H)--> [el];[un],{assert(elemento(H))}.
articulo(m,p,H)--> [los];[unos],{assert(elemento(H))}.

sustantivo(m,s,H)--> [helado];[chocolate];[carro],{assert(elemento(H))}.
sustantivo(m,p,H)--> [helados];[chocolates];[carros],{assert(elemento(H))}.
sustantivo(f,s,H)--> [camisa];[fresa];[cuchara];[cocina],{assert(elemento(H))}.
sustantivo(f,p,H)--> [camisas];[fresas];[cucharas],{assert(elemento(H))}.
nombre_personal(f,H)--> [ana];{assert(elemento(H))}.
nombre_personal(f,H)--> [valery],{assert(elemento(H))}.
nombre_personal(m,H)--> [carlos];[kevin],{assert(elemento(H))}.

adjetivo(m,s,H)--> [rico],{assert(elemento(H))}.
adjetivo(m,p,H)--> [ricos],{assert(elemento(H))}.
adjetivo(f,s,H)--> [linda],{assert(elemento(H))}.
adjetivo(f,p,H)--> [lindas],{assert(elemento(H))}.

%agregarAGrafoElemento(R,G).

conjuncion--> [y];[o].

%%
% Sintagma Verbal: Verbo + Predicado
% Conconrdandia de Verbo y Preposiciones
%%

sintagmaVerbal(G,N,H)-->(verboS(G,N,H));(verboE(G,N,H)).
sintagmaVerbal(G,N,H)-->(verboS(G,N,H),predicadoS(H));(verboE(G,N,H),predicadoE(H)).

verboS(_,s,H)--> [come];[tiene],{assert(elemento(H))}.
verboS(_,p,H)--> [comen];[tienen],{assert(elemento(H))}.
verboE(_,s,H)--> [corre];[camina],{assert(elemento(H))}.
verboE(_,p,H)--> [corren];[caminan],{assert(elemento(H))}.

predicado_directo(H)--> sintagmaNominal(_,_,H).

%ana y carlos comen
%ana y carlos comen un helado rico
%kevin y valery comen helado de fresa
%kevin y valery comen helado con cuchara
%carlos y ana tienen una camisa linda
predicadoS(H)--> predicado_directo(H).
predicadoS(H)--> predicado_indirectoS(H).
predicadoS(H)--> predicado_directo(H),predicado_indirectoS(H).
predicado_indirectoS(H)--> sintagmaNominal(_,_,H), preposicionS(H), sintagmaNominal(_,_,H).
%kevin y valery corren en la cocina
%Kevin y ana corren desde la cocina
%carlos y valery caminan hacia los carros
predicadoE(H)--> predicado_indirectoE(H).
predicado_indirectoE(H)-->preposicionE(H), sintagmaNominal(_,_,H).
preposicionS(H)--> [de];[con];[en],{assert(elemento(H))}.
preposicionE(H)--> [desde];[hacia];[en],{assert(elemento(H))}.











