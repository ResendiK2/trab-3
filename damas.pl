% Representação inicial do tabuleiro
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

% Exibir o tabuleiro
exibir_tabuleiro :-
    tabuleiro_inicial(Tabuleiro),
    imprimir_tabuleiro(Tabuleiro).

imprimir_tabuleiro([]).
imprimir_tabuleiro([Linha | Resto]) :-
    writeln(Linha),
    imprimir_tabuleiro(Resto).

% Movimento de uma peça (sem validações complexas)
mover(X1, Y1, X2, Y2) :-
    tabuleiro_inicial(Tabuleiro),
    mover_peca(Tabuleiro, X1, Y1, X2, Y2, NovoTabuleiro),
    imprimir_tabuleiro(NovoTabuleiro).

mover_peca(Tab, X1, Y1, X2, Y2, NovoTabuleiro) :-
    % Simplificação para mover uma peça de uma posição para outra
    substituir(Tab, X1, Y1, b, TabIntermediario),
    substituir(TabIntermediario, X2, Y2, j1, NovoTabuleiro).

substituir([Linha | Resto], 0, Col, Valor, [NovaLinha | Resto]) :-
    substituir_linha(Linha, Col, Valor, NovaLinha).
substituir([Linha | Resto], Lin, Col, Valor, [Linha | NovoResto]) :-
    Lin > 0,
    Lin1 is Lin - 1,
    substituir(Resto, Lin1, Col, Valor, NovoResto).

substituir_linha([_ | Resto], 0, Valor, [Valor | Resto]).
substituir_linha([Elem | Resto], Col, Valor, [Elem | NovoResto]) :-
    Col > 0,
    Col1 is Col - 1,
    substituir_linha(Resto, Col1, Valor, NovoResto).
