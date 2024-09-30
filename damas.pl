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

% Representacao inicial do tabuleiro (b = vazio, j1 = jogador 1, j2 = jogador 2)
tabuleiro_inicial([
    [b, j1, b, j1, b, j1, b, j1],
    [j1, b, j1, b, j1, b, j1, b],
    [b, j1, b, j1, b, j1, b, j1],
    [b, b, b, b, b, b, b, b],
    [b, b, b, b, b, b, b, b],
    [j2, b, j2, b, j2, b, j2, b],
    [b, j2, b, j2, b, j2, b, j2],
    [j2, b, j2, b, j2, b, j2, b]
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
        executar_movimento(Movimento, Tabuleiro, NovoTabuleiro),
        loop_jogo(NovoTabuleiro, maquina);
        writeln("Movimento invalido, tente novamente."),
        loop_jogo(Tabuleiro, jogador)
    ).

loop_jogo(Tabuleiro, maquina) :-
    writeln("Turno da Maquina"),
    exibir_tabuleiro(Tabuleiro),
    mover_maquina(Tabuleiro, NovoTabuleiro),
    writeln("Maquina fez seu movimento."),
    loop_jogo(NovoTabuleiro, jogador).

% Exemplo de funcao de movimento basico (sem validacao complexa)
validar_movimento(mv(Coord1, Coord2), Tabuleiro) :-
    coordenada_para_indice(Coord1, X1, Y1),
    coordenada_para_indice(Coord2, X2, Y2),
    dentro_dos_limites(X1, Y1),
    dentro_dos_limites(X2, Y2),
    nth0(X1, Tabuleiro, Linha1),
    nth0(Y1, Linha1, Peca),
    Peca \= b.

% Movimenta a peca (sem capturas por enquanto)
executar_movimento(mv(Coord1, Coord2), Tabuleiro, NovoTabuleiro) :-
    coordenada_para_indice(Coord1, X1, Y1),
    coordenada_para_indice(Coord2, X2, Y2),
    nth0(X1, Tabuleiro, Linha1),
    nth0(Y1, Linha1, Peca),
    substituir(Tabuleiro, X2, Y2, Peca, TabuleiroIntermediario),
    substituir(TabuleiroIntermediario, X1, Y1, b, NovoTabuleiro).

% Movimento basico da maquina (exemplo simplificado)
mover_maquina(Tabuleiro, NovoTabuleiro) :-
    executar_movimento(mv("f4", "e5"), Tabuleiro, NovoTabuleiro).

% Verifica se as coordenadas estao dentro dos limites do tabuleiro
dentro_dos_limites(X, Y) :-
    X >= 0, X < 8, Y >= 0, Y < 8.

% Conversao de coordenadas como "a1" para indices numericos
coordenada_para_indice(Coord, X, Y) :-
    atom_chars(Coord, [LetraColuna, Linha]),
    char_code(LetraColuna, ColunaASCII),
    X is 8 - (Linha - 48),  % Converte '1'-'8' para 0-7
    Y is ColunaASCII - 97.  % Converte 'a'-'h' para 0-7

% Funcao para substituir elementos na matriz (tabuleiro)
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
