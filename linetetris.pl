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
	faz_tabuleiro(M,N,L),
	imprime_tabuleiro(L),nl,nl,
	jogo(F),
	imprime_tabuleiro(Lf).
jogo(2).

imprime_tabuleiro([L]):-
	write(L),
	nl.
imprime_tabuleiro([L|Ls]):-
	write(L),
	nl,
	imprime_tabuleiro(Ls).

faz_tabuleiro(0,_,[]).
faz_tabuleiro(M,N,T) :-
	N>0,
	M>0,
	faz_linha(N,L),
	M1 is M-1,
	faz_tabuleiro(M1,N,T1),
	append(T1,[L],T).

faz_linha(1,[0]).
faz_linha(N,[0|L]):-
	N > 1,
	N1 is N-1,
	faz_linha(N1,L).


jogo(1) :-
	jogador_1(T,T1).

jogador_1(T,TabF1):-
	imprime_tabuleiro(T),
	write('jogador 1 é a sua vez!'),nl,
	write('Introduza o numero da coluna onde deseja jogar '),nl,
	read(C), nl,
	insere(1,C,T,TabF1),
	jogador_2(TabF1,TabF2).


insere(J,C,T,Tf):-
	length(T,Size),
	S is Size-1,
	nth0(S,T,L),
	C1 is C+1,
	substitui(C1,J,L,L1),
	substitui(Size,L1,T,Tf).
%%insere(1,1,[[1|Ls]|Tab]) :-

substitui(1,Y,[X|L],[Y|L]).
substitui(N,Y,[L|Ls],[L|LN]) :-
	N1 is N-1,
	substitui(N1,Y,Ls,LN).









