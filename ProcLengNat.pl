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
sintagmaNominal(G,N,H)--> articulo(G,N),sustantivo(G,N,H).
sintagmaNominal(G,N,H)--> sustantivo(G,N,H), adjetivo(G,N,H).
sintagmaNominal(G,N,H)--> articulo(G,N), adjetivo(G,N,H),sustantivo(G,N,H).
sintagmaNominal(m,p,H)--> nombre_personal(_,H),conjuncion,sintagmaNominal(_,_,H).
sintagmaNominal(m,p,H)--> articulo(G,N),sustantivo(G,N,H),conjuncion,sintagmaNominal(_,_,H).

%oracion2-->sintagmaNominal2(G,N),sintagmaVerbal2(G,N).
% sintagmaNominal2(G,N)--> (articulo(G,N),sustantivo2(_));
% (nombrePersonal2(_),conjuncion,sintagmaNominal2(_,_)).


:-dynamic arco/2.

unifica(E1,E2,E2).


%agregar(H,[]).
%agregar(H,E):-
%agregar(H|E, []).

articulo(f,s)--> [una],[la].
articulo(f,p)--> [las];[unas].
articulo(m,s)--> [el];[un].
articulo(m,p)--> [los];[unos].

sustantivo(m,s,H)--> [helado];[chocolate];[carro],{unifica(H, "helado",H)}.
sustantivo(m,p,H)--> [helados];[chocolates];[carros],{unifica(H,"helados",H)}.
sustantivo(f,s,H)--> [camisa];[fresa];[cuchara];[cocina],{unifica(H,"camisa",H)}.
sustantivo(f,p,H)--> [camisas];[fresas];[cucharas],{unifica(H,"camisas",H)}.
nombre_personal(f,H)--> [ana];{unifica(H,"ana",H)}.
nombre_personal(f,H)--> [valery],{unifica(H,"valery",H)}.
nombre_personal(m,H)--> [carlos];[kevin],{unifica(H,"carlos",H)}.

adjetivo(m,s,H)--> [rico],{unifica(H,"rico",H)}.
adjetivo(m,p,H)--> [ricos],{unifica(H,"ricos",H)}.
adjetivo(f,s,H)--> [linda],{unifica(H,"linda",H)}.
adjetivo(f,p,H)--> [lindas],{unifica(H,"",H)}.

%agregarAGrafoElemento(R,G).

conjuncion--> [y];[o].

%%
% Sintagma Verbal: Verbo + Predicado
% Conconrdandia de Verbo y Preposiciones
%%

sintagmaVerbal(G,N,H)-->(verboS(G,N,H));(verboE(G,N,H)).
sintagmaVerbal(G,N,H)-->(verboS(G,N,H),predicadoS(H));(verboE(G,N,H),predicadoE(H)).

verboS(_,s,H)--> [come];[tiene],{H is "come"}.
verboS(_,p,H)--> [comen];[tienen],{H is "comen"}.
verboE(_,s,H)--> [corre];[camina],{H is "corre"}.
verboE(_,p,H)--> [corren];[caminan],{H is "corren"}.

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
predicado_indirectoE(H)--> preposicionE(H), sintagmaNominal(_,_,H).
preposicionS(H)--> [de];[con];[en],{H is "de"}.
preposicionE(H)--> [desde];[hacia];[en],{H is "desde"}.











