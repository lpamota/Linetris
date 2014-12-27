menu :-
	write('0- Sair'),nl,
	write('1- Humano vs Humano'), nl,
	read(N),
	jogo(N).


% HUMANO VS HUMANO
jogo(1):-
	write(' Introduza o numero de colunas do tabuleiro (no minimo 4) '),nl,
	read(N),
	write(' Introduza o numero de linhas do tabuleiro (minimo 4) '),nl,
	read(M),
	write(' Indique o numero do jogador que começa a jogar (1 ou 2) : '),nl,
	read(F),
	M>3, 				%% para certificar que o tabuleiro tem no minimo 4 x 4
	faz_tabuleiro(M,N,L),            % M - numero de linhas, N numero colunas, L tabuleiro
	imprime_tabuleiro(L),nl,nl,      
	jog_inicial(F,L),                 %quem começa a jogar
	imprime_tabuleiro(L).

%HUMANO VS PC
jogo(2).

imprime_tabuleiro([]).

imprime_tabuleiro([L|Ls]):-
	imprime_linha(L),nl,
	imprime_tabuleiro(Ls).

imprime_linha([]).

imprime_linha([E|Es]):-     
	atomic(E),          %verifica se o elemento E é atomico (diferente de uma variavel)
	write(' '),           % e imprime-o
	write(E),
	write(' '),
	imprime_linha(Es).

imprime_linha([E|Es]):-
	\+ atomic(E),        % Se o elemento E for variavel, imprime '_'
	write(' _ '),
	imprime_linha(Es).

faz_tabuleiro(0,_,[]).
faz_tabuleiro(M,N,T) :-
	N>0,
	M>0,
	faz_linha(N,L),            %faz uma linha L com N colunas 
	M1 is M-1,                 % retira um linha, pois ja esta contruida e pronta a adicionar
	faz_tabuleiro(M1,N,T1),    %volta a fazer o tabuleiro desta vez com -1 linha
	append(T1,[L],T).          % junta todas as linhas L, no tabuleiro T


faz_linha(N,L):-                %cria uma linha L com N colunas
	N > 3, 			%para certificar que o tabuleiro é 4x4 no minimo
 	length(L,N).  		%cria uma linha L de variaveis nao instanciadas, com tamanho N


jog_inicial(1,T) :-        %inicia o jogo o jogador 1
	jogador_1(T).

jog_inicial(2,T) :-         %inicia o jogo o jog 2
	jogador_2(T).

jogador_1(T):-
	imprime_tabuleiro(T),
	write('jogador 1 é a sua vez!'),nl,
	write('Introduza o numero da coluna onde deseja jogar '),nl,
	read(C), nl,                   %lê a coluna onde o jogador quer jogar
 	length(T,Size), 		%guarda em Size o tamanho do tabuleiro T e
	S is Size-1, 			% S é Size-1 para que quando for inserir no tabuleiro não inserir numa posiçao
	insere(1,S,C,T),		%inexistente, pois os indices do Tabuleiro vao de 0 a S
	jogador_2(T).                   % quando o jogador 1 termina a jogada passa a ser a vez do jogador 2 jogar

jogador_2(T):-
	imprime_tabuleiro(T),
	write('jogador 2 é a sua vez!'),nl,
	write('Introduza o numero da coluna onde deseja jogar '),nl,
	read(C), nl,
	length(T,Size),
	S is Size-1,
	insere(2,S,C,T),
	jogador_1(T).

%insere uma ficha J no tabuleiro, neste predicado, verifica se já foi jogado na posicao onde quer inserir,
%pois caso ja tenha sido terei de jogar por cima
insere(J,S,C,T):-            %J é o numero do jogador, S o numero da ultima linha do tabuleiro T, e C a coluna onde o
	C1 is C-1, 		%jogador quer jogar, retira-se 1 a C, pois ao usar nth0 as linhas começam no indice 0
	nth0(S,T,L), 		%vai guardar em L a ultima linha do tabuleiro
	nth0(C1,L,X), 		% guarda o elemento presente na posiçao C1 da linha L em X
	atomic(X),  		% se X for atomico significa que já foi jogada uma peça nesse sitio
	!,  			% pelo que vai ter de jogar na linha acima
	S1 is S-1,  		%correspondente a S-1
	inserel(J,S1,C1,T). 	%insere na linha superior, caso seja possivel, ou entao noutra, mas na mesma coluna

%este predicado só é usado se o anterior falhar, o que significa que ainda não foi jogado nessa posicao e posso inserir
%de imediato na posicao
insere(J,S,C,T):-  
	C1 is C-1,      %numero da coluna a inserir, pois começa em 0
	nth0(S,T,L),    % guarda em L a ultima linha do tabuleiro
	nth0(C1,L,J).   % unifica a variavel na posicao C1 da linha L com J

% igual ao primeiro insere/4 so que como vai verificando se esta alguma ficha na posicao ate poder jogar
% tem de garantir que nao insere fora do tabuleiro isto é quando S for menor que 0
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
%igual ao segundo insere/4 mas garante que nao insere fora do tabuleiro, quando S menor que 0
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
% dadas as posicoes onde foi inserida a ultima ficha, verifica se existe 4 em linha na vertical, isto é,
% se nas 3 posicoes abaixo tem fichas do mesmo jogador
verifica_vertical(J,S,C,T) :-
	S1 is S+1,           %S1 corresponde ao numero da linha abaixo da linha onde foi jogada a ultima peça
	nth0(S1,T,L1),       	% guarda em L1 a linha correspondente a posicao S1 do tabuleiro T
	nth0(C,L1,J),		% verifica se na posicao C da linha L1 esta uma ficha do jogador que jogou
	S2 is S1+1,             %
	nth0(S2,T,L2),		%repete o processo anterior mas na linha abaixo
	nth0(C,L2,J),		%
	S3 is S2+1, 		
	nth0(S3,T,L3),		%volta a fazer o mesmo na linha abaixo da anterior
	nth0(C,L3,J).

%verifica se ha 4 em linha na horizontal, em redor da posicao onde a ultima ficha do jogador J foi jogada 
verifica_horizontal(J,S,C,T) :-
	nth0(S,T,L1),			%L1 corresponde á linha onde a peça foi jogada
	C1 is C-1,                      %C1 é o numero da coluna antes da coluna onde foi jogada a peça 
	C2 is C-2, 			%C2 2 colunas antes
	C3 is C-3, 			%C3 3 colunas a menos
	C4 is C+1, 			% C4 coluna seguinte
	C5 is C+2, 			%C5 2 colunas a mais
	C6 is C+3,                      %C6 coluna mais 3
	(( nth0(C3,L1,J), nth0(C2,L1,J), nth0(C1,L1,J));   %ve se em C3 C2 e C1 estao peças do mesmo jogador
	(   nth0(C2,L1,J), nth0(C1,L1,J), nth0(C4,L1,J));  % ve em C2 C1 e C4 
	(   nth0(C5,L1,J), nth0(C4,L1,J), nth0(C1,L1,J));  % ve em C4 C5 e C1
	(   nth0(C6,L1,J), nth0(C5,L1,J), nth0(C4,L1,J))). % ve em C6, C5 e C4, caso pelo menos um dos casos se verifique
								% da true, pois ha 4 em linha

verifica_diagonal(J,S,C,T) :-
	S3 is S-3,                         %% tá bemm mas
	S2 is S-2,			   %%%%Dá 2 respostas nao sei porque
	S1 is S-1,                         %%
	S4 is S+1, 			% S's correspondem aos numeros das linhas acima ou abaixo da linha onde foi jogado
	S5 is S+2, 
	S6 is S+3,
	C1 is C-1,  			%C's correspondem as colunas anteriores ou depois
	C2 is C-2,			% a coluna onde a peca foi jogada
	C3 is C-3,
	C4 is C+1,
	C5 is C+2,
	C6 is C+3,
	(   (nth0(S3,T,L3), nth0(C3,L3,J),
	     nth0(S2,T,L2), nth0(C2,L2,J),			%diagonal crescente para a esquerda em que
	     nth0(S1,T,L1), nth0(C1,L1,J), write('1'),nl); 	%a ultima peca jogada e a ultima peca

	    (nth0(S2,T,L2), nth0(C2,L2,J),                      %diagonal crescente para a esquerda
	     nth0(S1,T,L1), nth0(C1,L1,J), 			%ultima peca jogada e a penultima
	     nth0(S4,T,L4), nth0(C4,L1,J),write('2'),nl);

	    (nth0(S5,T,L5), nth0(C5,L5,J), 			%diagonal crescente para a esq
	     nth0(S4,T,L4), nth0(C4,L4,J),  			% ultima peca jogada e a segunda
	     nth0(S1,T,L1), nth0(C1,L1,J),write('3'),nl);

	    (nth0(S6,T,L6), nth0(C6,L6,J), 			%diagonal crescente pra esq
	     nth0(S5,T,L5), nth0(C5,L5,J), 			%ultima peca jogada e a primeira
	     nth0(S4,T,L4), nth0(C4,L4,J),write('4'),nl);

	    (nth0(S3,T,L3), nth0(C6,L3,J), 			%diagonal decrescente pra esq
	     nth0(S2,T,L2), nth0(C5,L2,J), 			%ultima peca jogada e a ultima peca
	     nth0(S1,T,L1), nth0(C4,L1,J), write('5'),nl);

	    (nth0(S2,T,L2), nth0(C5,L2,J), 			%diagonal decrescente
	     nth0(S1,T,L1), nth0(C4,L1,J), 			%ultima peca jogada e a penultima
	     nth0(S4,T,L4), nth0(C1,L1,J),write('6'),nl); 

	    (nth0(S5,T,L5), nth0(C2,L5,J), 			%diagonal decrescente
	     nth0(S4,T,L4), nth0(C1,L4,J), 			%ultima peca jogada e a segunda
	     nth0(S1,T,L1), nth0(C4,L1,J),write('7'),nl);

	    (nth0(S6,T,L6), nth0(C3,L6,J), 			%diagonal decrescente
	     nth0(S5,T,L5), nth0(C2,L5,J),			%ultima peca jogada e a primeira
	     nth0(S4,T,L4), nth0(C1,L4,J)),write('8'),nl),
	write('tatatatata').




elimina_linha(T) :-






