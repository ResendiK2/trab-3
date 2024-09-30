% Inicio do jogo
iniciar :-
    writeln("Bem-vindo ao Jogo de Damas!"),
    writeln("O objetivo e implementar um jogo de damas em Prolog."),
    writeln("O jogo comeca agora."),
    writeln("Escolha quem comeca:"),
    writeln("1. Jogador vs Maquina"),
    writeln("2. Maquina vs Maquina"),
    read(Opcao),  % Captura a opcao diretamente
    processar_opcao(Opcao).

% Processar a escolha do jogador
processar_opcao(1) :-
    writeln("Voce escolheu Jogador vs Maquina"),
    tabuleiro_inicial(Tabuleiro),
    loop_jogo(Tabuleiro, jogador).
processar_opcao(2) :-
    writeln("Voce escolheu Maquina vs Maquina"),
    tabuleiro_inicial(Tabuleiro),
    loop_jogo(Tabuleiro, maquina).
processar_opcao(_) :-
    writeln("Opcao invalida. Tente novamente."),
    iniciar.

% Representacao inicial do tabuleiro
% 1 = casas não jogáveis, 0 = casas jogáveis vazias, a = jogador 1, b = jogador 2
tabuleiro_inicial([
    [1, a, 1, a, 1, a, 1, a],
    [a, 1, a, 1, a, 1, a, 1],
    [1, a, 1, a, 1, a, 1, a],
    [0, 1, 0, 1, 0, 1, 0, 1],
    [1, 0, 1, 0, 1, 0, 1, 0],
    [b, 1, b, 1, b, 1, b, 1],
    [1, b, 1, b, 1, b, 1, b],
    [b, 1, b, 1, b, 1, b, 1]
]).

% Exibe o tabuleiro no terminal
exibir_tabuleiro([]).
exibir_tabuleiro([Linha | Resto]) :-
    writeln(Linha),
    exibir_tabuleiro(Resto).

% Loop do jogo, alterna entre jogador e maquina
loop_jogo(Tabuleiro, jogador) :-
    writeln("Turno do Jogador"),
    exibir_tabuleiro(Tabuleiro),
    writeln("Digite o movimento no formato mv(COORD1,COORD2)."),
    writeln("Ou use cap(COORD1,[COORD2,...]) para capturas."),
    read(Movimento),
    (validar_movimento(Movimento, Tabuleiro) ->
        (executar_movimento(Movimento, Tabuleiro, NovoTabuleiro),
         loop_jogo(NovoTabuleiro, maquina)
        )
    ;   (writeln("Movimento invalido, tente novamente."),
         loop_jogo(Tabuleiro, jogador)
        )
    ).

loop_jogo(Tabuleiro, maquina) :-
    writeln("Turno da Maquina..."),
    sleep(1),  % Delay para visualização
    mover_maquina_aleatoria(Tabuleiro, NovoTabuleiro),
    writeln("Maquina fez sua jogada."),
    exibir_tabuleiro(NovoTabuleiro),
    loop_jogo(NovoTabuleiro, jogador).

% Movimentacao aleatória da maquina
mover_maquina_aleatoria(Tabuleiro, NovoTabuleiro) :-
    findall((X1, Y1, X2, Y2), movimento_valido(X1, Y1, X2, Y2, Tabuleiro), Movimentos),
    random_member((X1, Y1, X2, Y2), Movimentos),
    format("Maquina move de ~w para ~w~n", [(X1,Y1), (X2,Y2)]),
    executar_movimento(mv(X1, Y1, X2, Y2), Tabuleiro, NovoTabuleiro).

% Valida os movimentos simples e captura
validar_movimento(mv(Coord1, Coord2), Tabuleiro) :-
    coordenada_para_indice(Coord1, X1, Y1),
    coordenada_para_indice(Coord2, X2, Y2),
    dentro_dos_limites(X1, Y1),
    dentro_dos_limites(X2, Y2),
    nth0(X1, Tabuleiro, Linha1),
    nth0(Y1, Linha1, Peca),
    Peca \= 1,  % Certifica que a casa inicial não é inválida
    nth0(X2, Tabuleiro, Linha2),
    nth0(Y2, Linha2, Destino),
    Destino == 0,  % Certifica que a casa final está vazia.

validar_movimento(cap(Coord1, [Coord2|Capturas]), Tabuleiro) :-
    validar_captura(Coord1, Coord2, Tabuleiro),
    validar_movimento(cap(Coord2, Capturas), Tabuleiro).
validar_movimento(cap(_, []), _).  % Última posição

% Valida a captura entre Coord1 e Coord2
validar_captura(Coord1, Coord2, Tabuleiro) :-
    coordenada_para_indice(Coord1, X1, Y1),
    coordenada_para_indice(Coord2, X2, Y2),
    dentro_dos_limites(X1, Y1),
    dentro_dos_limites(X2, Y2),
    distancia(Coord1, Coord2, Dx, Dy),
    Dx == 2, Dy == 2,  % Certifica que é um salto de captura
    nth0(X1, Tabuleiro, Linha1),
    nth0(Y1, Linha1, Peca),
    nth0(X2, Tabuleiro, Linha2),
    nth0(Y2, Linha2, Destino),
    Destino == 0,  % A casa final deve estar vazia
    XInter is (X1 + X2) // 2,
    YInter is (Y1 + Y2) // 2,
    nth0(XInter, Tabuleiro, LinhaInter),
    nth0(YInter, LinhaInter, Oponente),
    Oponente \= 0,  % A casa intermediária deve conter uma peça adversária
    Oponente \= Peca.

% Funções auxiliares para movimentação

% Executa um movimento válido
executar_movimento(mv(Coord1, Coord2), Tabuleiro, NovoTabuleiro) :-
    coordenada_para_indice(Coord1, X1, Y1),
    coordenada_para_indice(Coord2, X2, Y2),
    nth0(X1, Tabuleiro, Linha1),
    nth0(Y1, Linha1, Peca),
    substituir(Tabuleiro, X2, Y2, Peca, TabuleiroIntermediario),
    substituir(TabuleiroIntermediario, X1, Y1, 0, NovoTabuleiro),
    promover_dama(NovoTabuleiro, X2, Y2, NovoTabuleiro).

% Executa uma captura (removendo a peça capturada)
executar_movimento(cap(Coord1, [Coord2 | Capturas]), Tabuleiro, NovoTabuleiro) :-
    coordenada_para_indice(Coord1, X1, Y1),
    coordenada_para_indice(Coord2, X2, Y2),
    XInter is (X1 + X2) // 2,
    YInter is (Y1 + Y2) // 2,
    substituir(Tabuleiro, XInter, YInter, 0, TabuleiroIntermediario),
    executar_movimento(mv(Coord1, Coord2), TabuleiroIntermediario, TabuleiroFinal),
    executar_movimento(cap(Coord2, Capturas), TabuleiroFinal, NovoTabuleiro).
executar_movimento(cap(_, []), Tabuleiro, Tabuleiro).  % Último passo da captura

% Verifica se a peça foi promovida a dama
promover_dama(Tabuleiro, X2, Y2, NovoTabuleiro) :-
    nth0(X2, Tabuleiro, Linha),
    nth0(Y2, Linha, Peca),
    (X2 == 0, Peca == a -> substituir(Tabuleiro, X2, Y2, 'A', NovoTabuleiro);
     X2 == 7, Peca == b -> substituir(Tabuleiro, X2, Y2, 'B', NovoTabuleiro);
     NovoTabuleiro = Tabuleiro).

% Verifica se as coordenadas estão dentro dos limites do tabuleiro
dentro_dos_limites(X, Y) :-
    X >= 0, X < 8, Y >= 0, Y < 8.

% Conversão de coordenadas como "a3" para índices numéricos
coordenada_para_indice(Coord, X, Y) :-
    atom_chars(Coord, [LetraColuna, Linha]),
    char_code(LetraColuna, ColunaASCII),
    X is 8 - (Linha - 48),  % Converte '1'-'8' para 0-7
    Y is ColunaASCII - 97.  % Converte 'a'-'h' para 0-7

% Função para substituir elementos no tabuleiro
substituir([Linha | Resto], 0, Coluna, Valor, [NovaLinha | Resto]) :-
    substituir_linha(Linha, Coluna, Valor, NovaLinha).
substituir([Linha | Resto], LinhaAlvo, Coluna, Valor, [Linha | NovoResto]) :-
    LinhaAlvo > 0,
    LinhaAlvo1 is LinhaAlvo - 1,
    substituir(Resto, LinhaAlvo1, Coluna, Valor, NovoResto).

substituir_linha([_ | Resto], 0, Valor, [Valor | Resto]).
substituir_linha([Elemento | Resto], Coluna, Valor, [Elemento | NovoResto]) :-
    Coluna > 0,
    Coluna1 is Coluna - 1,
    substituir_linha(Resto, Coluna1, Valor, NovoResto).

% Movimentos válidos para a máquina (todas as direções)
movimento_valido(X1, Y1, X2, Y2, Tabuleiro) :-
    dentro_dos_limites(X1, Y1),
    dentro_dos_limites(X2, Y2),
    nth0(X1, Tabuleiro, Linha1),
    nth0(Y1, Linha1, Peca),
    Peca \= 1,
    nth0(X2, Tabuleiro, Linha2),
    nth0(Y2, Linha2, Destino),
    Destino == 0.
