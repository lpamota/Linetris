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
	length(T,Size),
	S is Size-1,
	insere(1,S,C,T),
	jogador_2(T).

jogador_2(T):-
	imprime_tabuleiro(T),
	write('jogador 2 é a sua vez!'),nl,
	write('Introduza o numero da coluna onde deseja jogar '),nl,
	read(C), nl,
	length(T,Size),
	S is Size-1,
	insere(2,S,C,T),
	jogador_1(T).


insere(J,S,C,T):-
	C1 is C-1,
	nth0(S,T,L),
	nth0(C1,L,X),
	atomic(X),
	!,
	S1 is S-1,
	inserel(J,S1,C1,T).


insere(J,S,C,T):-
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

%inserel(J,S,C,T):-
	%S>=0,
	%nth0(S,T,L),
	%nth0(C,L,J),
	%\+ verifica(J,S,C,T),!.

inserel(J,S,C,T):-
	S>=0,
	nth0(S,T,L),
	nth0(C,L,J).%,
	%verifica(J,S,C,T).


verifica(J,S,C,T) :-
	write("aijefioej"),nl,
	verifica_vertical(J,S,C,T),
	write("O jogador: "),
	write(J),
	write(" venceu!").

%verifica(J,S,C,T) :-
	%verifica_horizontal(J,S,C,T),
	%write("O jogador: "),
	%write(J),
	%write(" venceu!").

%verifica(J,S,C,T) :-
	%verifica_diagonal(J,S,C,T),
	%write("O jogador: "),
	%write(J),
	%write(" venceu!").

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
	nth0(S,T,L1),
	C1 is C-1,
	C2 is C-2,
	C3 is C-3,
	C4 is C+1,
	C5 is C+2,
	C6 is C+3,
	(( nth0(C3,L1,J), nth0(C2,L1,J), nth0(C1,L1,J));
	(   nth0(C2,L1,J), nth0(C1,L1,J), nth0(C4,L1,J));
	(   nth0(C5,L1,J), nth0(C4,L1,J), nth0(C1,L1,J));
	(   nth0(C6,L1,J), nth0(C5,L1,J), nth0(C4,L1,J))).


verifica_diagonal(J,S,C,T) :-
	S3 is S-3,                         %% tá bemm mas
	S2 is S-2,			   %%%%Dá 2 respostas nao sei porque
	S1 is S-1,                         %%
	S4 is S+1,
	S5 is S+2,
	S6 is S+3,
	C1 is C-1,
	C2 is C-2,
	C3 is C-3,
	C4 is C+1,
	C5 is C+2,
	C6 is C+3,
	(   (nth0(S3,T,L3), nth0(C3,L3,J),
	     nth0(S2,T,L2), nth0(C2,L2,J),
	     nth0(S1,T,L1), nth0(C1,L1,J), write('1'),nl);

	    (nth0(S2,T,L2), nth0(C2,L2,J),
	     nth0(S1,T,L1), nth0(C1,L1,J),
	     nth0(S4,T,L4), nth0(C4,L1,J),write('2'),nl);

	    (nth0(S5,T,L5), nth0(C5,L5,J),
	     nth0(S4,T,L4), nth0(C4,L4,J),
	     nth0(S1,T,L1), nth0(C1,L1,J),write('3'),nl);

	    (nth0(S6,T,L6), nth0(C6,L6,J),
	     nth0(S5,T,L5), nth0(C5,L5,J),
	     nth0(S4,T,L4), nth0(C4,L4,J),write('4'),nl);

	    (nth0(S3,T,L3), nth0(C6,L3,J),
	     nth0(S2,T,L2), nth0(C5,L2,J),
	     nth0(S1,T,L1), nth0(C4,L1,J), write('5'),nl);

	    (nth0(S2,T,L2), nth0(C5,L2,J),
	     nth0(S1,T,L1), nth0(C4,L1,J),
	     nth0(S4,T,L4), nth0(C1,L1,J),write('6'),nl);

	    (nth0(S5,T,L5), nth0(C2,L5,J),
	     nth0(S4,T,L4), nth0(C1,L4,J),
	     nth0(S1,T,L1), nth0(C4,L1,J),write('7'),nl);

	    (nth0(S6,T,L6), nth0(C3,L6,J),
	     nth0(S5,T,L5), nth0(C2,L5,J),
	     nth0(S4,T,L4), nth0(C1,L4,J)),write('8'),nl),
	write('tatatatata').




elimina_linha(T) :-






