:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_files)).

% Definindo aliases para servir arquivos estáticos
:- multifile http:location/3.
:- dynamic   http:location/3.

http:location(css, '/css', []).
http:location(js, '/js', []).

% Inicializa o servidor e abre a interface HTML
iniciar :-
    writeln("Bem-vindo ao Jogo de Damas!"),
    writeln("Escolha o modo de jogo:"),
    writeln("1. Jogar pelo terminal"),
    writeln("2. Jogar pela interface grafica"),
    read(Opcao),
    processar_opcao(Opcao).

% Processar a escolha do jogador
processar_opcao(1) :- iniciar_jogo_terminal.
processar_opcao(2) :- iniciar_jogo_interface.
processar_opcao(_) :-
    writeln("Opcao invalida. Tente novamente."),
    iniciar.

% Inicia o jogo no terminal
iniciar_jogo_terminal :-
    writeln("Voce escolheu jogar pelo terminal"),
    tabuleiro_inicial(Tabuleiro),
    loop_jogo(Tabuleiro, jogador).

% Inicia o jogo na interface gráfica (servidor HTTP)
iniciar_jogo_interface :-
    writeln("Iniciando o servidor na porta 8000..."),
    http_server(http_dispatch, [port(8000)]),
    writeln("Servidor rodando. Abra o navegador em http://localhost:8000.").

% Servindo os arquivos CSS e JS
:- http_handler(css(.), serve_files_in_directory('static/css'), [prefix]).
:- http_handler(js(.), serve_files_in_directory('static/js'), [prefix]).

% Rota para carregar a página inicial da interface gráfica
:- http_handler(root(.), interface_damas_handler, []).

% Manipulador para carregar a interface gráfica
interface_damas_handler(_Request) :-
    reply_html_page(
        title('Jogo de Damas'),
        [ \html_requires(css('style.css')),
          h1('Jogo de Damas'),
          div([id=tabuleiro], ''),
          script([src('/js/script.js')], [])
        ]).

% Representação inicial do tabuleiro (0 = vazio, a = jogador 1, b = jogador 2)
tabuleiro_inicial([
    [0, a, 0, a, 0, a, 0, a],
    [a, 0, a, 0, a, 0, a, 0],
    [0, a, 0, a, 0, a, 0, a],
    [0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0],
    [b, 0, b, 0, b, 0, b, 0],
    [0, b, 0, b, 0, b, 0, b],
    [b, 0, b, 0, b, 0, b, 0]
]).

% Exibe o tabuleiro com numeração nas linhas e letras nas colunas (somente embaixo)
exibir_tabuleiro(Tabuleiro) :-
    exibir_linhas(Tabuleiro, 8),  % Começa a exibir as linhas a partir de 8
    writeln("  A  B  C  D  E  F  G  H").  % Letras nas colunas (somente na parte inferior)

% Exibe as linhas do tabuleiro com a numeração correspondente
exibir_linhas([], 0).
exibir_linhas([Linha | Resto], NumeroLinha) :-
    format('~w ', [NumeroLinha]),  % Adiciona o número da linha à esquerda
    exibir_linha_formatada(Linha),  % Exibe a linha do tabuleiro com formatação
    nl,  % Nova linha
    NovoNumeroLinha is NumeroLinha - 1,  % Decrementa o número da linha
    exibir_linhas(Resto, NovoNumeroLinha).

% Exibe uma linha do tabuleiro com formatação de largura fixa para manter o alinhamento
exibir_linha_formatada([]).
exibir_linha_formatada([Peca | Resto]) :-
    format('~|~w~3+', [Peca]),  % Exibe cada peça ocupando 3 caracteres de largura
    exibir_linha_formatada(Resto).


% Loop do jogo, alterna entre jogador e maquina (terminal)
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

% Validação do movimento
validar_movimento(mv(Coord1, Coord2), Tabuleiro) :-
    coordenada_para_indice(Coord1, X1, Y1),
    coordenada_para_indice(Coord2, X2, Y2),
    dentro_dos_limites(X1, Y1),
    dentro_dos_limites(X2, Y2),
    nth0(X1, Tabuleiro, Linha1),
    nth0(Y1, Linha1, Peca),
    Peca \= b.

% Movimentar peça
executar_movimento(mv(Coord1, Coord2), Tabuleiro, NovoTabuleiro) :-
    coordenada_para_indice(Coord1, X1, Y1),
    coordenada_para_indice(Coord2, X2, Y2),
    nth0(X1, Tabuleiro, Linha1),
    nth0(Y1, Linha1, Peca),
    substituir(Tabuleiro, X2, Y2, Peca, TabuleiroIntermediario),
    substituir(TabuleiroIntermediario, X1, Y1, b, NovoTabuleiro).

% Movimento da máquina
mover_maquina(Tabuleiro, NovoTabuleiro) :-
    executar_movimento(mv("f4", "e5"), Tabuleiro, NovoTabuleiro).

% Verifica se as coordenadas estão dentro dos limites do tabuleiro
dentro_dos_limites(X, Y) :-
    X >= 0, X < 8, Y >= 0, Y < 8.

% Conversão de coordenadas
coordenada_para_indice(Coord, X, Y) :-
    atom_chars(Coord, [LetraColuna, Linha]),
    char_code(LetraColuna, ColunaASCII),
    X is 8 - (Linha - 48),
    Y is ColunaASCII - 97.

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
