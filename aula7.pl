%SÉRIE 6

natural_number(0).
natural_number(s(X)):-natural_number(X).

%4.1

lista_ligada(fim).
lista_ligada(lista(_X,L)):-lista_ligada(L).

%4.2

elementos(fim,[]).
elementos(lista(X,L),[X|Xs]):-elementos(L,Xs).

%4.3

lista_ligada_inteiros(fim).
lista_ligada_inteiros(lista(X,L)):-integer(X),lista_ligada_inteiros(L).

%4.4 1.0

ordenada_cres(fim).
ordenada_cres(lista(_X,fim)).
ordenada_cres(lista(X,lista(Y,L))):- X<Y,ordenada_cres(lista(Y,L)).

ordenada_decres(fim).
ordenada_decres(lista(_X,fim)).
ordenada_decres(lista(X,lista(Y,L))):- X>Y,ordenada_decres(lista(Y,L)).

%4.4 2.0

ordenada2(fim,_S).
ordenada2(lista(_X,fim),_S).
ordenada2(lista(X,lista(Y,L)),S):- Z=..[S,X,Y], call(Z), ordenada2(lista(Y,L),S).

%3

conjunto([]).
%conjunto([_X]).
conjunto([X|Xs]):- 
	\+member(X,Xs),
	conjunto(Xs).