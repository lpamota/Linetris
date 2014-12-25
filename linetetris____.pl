menu :-
	write('0- Sair'),nl,
	write('1- Humano vs Humano'), nl,
	read(N),
	jogo(N).

jogo(1):-
	write(' Introduza o numero de colunas do tabuleiro (no minimo 4) '),nl,
	read(N),
	write(' Introduza o numero de linhas do tabuleiro (minimo 4) '),nl,
	read(M),
	write(' Indique o numero do jogador que começa a jogar (1 ou 2) : '),nl,
	read(F),
	M>3,
	faz_tabuleiro(M,N,L),
	imprime_tabuleiro(L),nl,nl,
	jog_inicial(F,L),
	imprime_tabuleiro(L).
jogo(2).

imprime_tabuleiro([]).

imprime_tabuleiro([L|Ls]):-
	imprime_linha(L),nl,
	imprime_tabuleiro(Ls).

imprime_linha([]).

imprime_linha([E|Es]):-
	atomic(E),
	write(' '),
	write(E),
	write(' '),
	imprime_linha(Es).

imprime_linha([E|Es]):-
	\+ atomic(E),
	write(' _ '),
	imprime_linha(Es).

faz_tabuleiro(0,_,[]).
faz_tabuleiro(M,N,T) :-
	N>0,
	M>0,
	faz_linha(N,L),
	M1 is M-1,
	faz_tabuleiro(M1,N,T1),
	append(T1,[L],T).


faz_linha(N,L):-
	N > 3,
	length(L,N).


jog_inicial(1,T) :-
	jogador_1(T).

jog_inicial(2,T) :-
	jogador_2(T).

jogador_1(T):-
	imprime_tabuleiro(T),
	write('jogador 1 é a sua vez!'),nl,
	write('Introduza o numero da coluna onde deseja jogar '),nl,
	read(C), nl,
	insere(1,C,T),
	jogador_2(T).

jogador_2(T):-
	imprime_tabuleiro(T),
	write('jogador 2 é a sua vez!'),nl,
	write('Introduza o numero da coluna onde deseja jogar '),nl,
	read(C), nl,
	insere(2,C,T),
	jogador_1(T).


insere(J,C,T):-
	length(T,Size),
	S is Size-1,
	C1 is C-1,
	nth0(S,T,L),
	nth0(C1,L,X),
	atomic(X),
	!,
	S1 is S-1,
	inserel(J,S1,C1,T).


insere(J,C,T):-
	length(T,Size),
	S is Size-1,
	C1 is C-1,
	nth0(S,T,L),
	nth0(C1,L,J).

inserel(J,S,C,T) :-
	S>=0,
	nth0(S,T,L),
	nth0(C,L,X),
	atomic(X),
	!,
	S1 is S-1,
	inserel(J,S1,C,T).

inserel(J,S,C,T):-
	S>=0,
	nth0(S,T,L),
	nth0(C,L,J),
	verifica(J,S,C,T).

verifica(J,S,C,T) :-
	verifica_vertical(J,S,C,T),
	write("O jogador: "),
	write(J),
	write(" venceu!").

verifica(J,S,C,T) :-
	verifica_horizontal(J,S,C,T),
	write("O jogador: "),
	write(J),
	write(" venceu!").

verifica(J,S,C,T) :-
	verifica_diagonal(J,S,C,T),
	write("O jogador: "),
	write(J),
	write(" venceu!").

verifica_vertical(J,S,C,T) :-
	S1 is S+1,
	nth0(S1,T,L1),
	nth0(C,L1,J),
	S2 is S1+1,
	nth0(S2,T,L2),
	nth0(C,L2,J),
	S3 is S2+1,
	nth0(S3,T,L3),
	nth0(C,L3,J).


verifica_horizontal(J,S,C,T) :-








