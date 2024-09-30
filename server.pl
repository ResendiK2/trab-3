:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).

:- http_handler('/move', move_handler, []).  % Manipula o endpoint /move

% Inicia o servidor na porta 8000
server(Port) :-
    http_server(http_dispatch, [port(Port)]).

% Manipulador de requisições de movimento
move_handler(Request) :-
    http_read_json_dict(Request, Data),
    % Aqui a lógica para validar e realizar o movimento seria inserida
    % Exemplo simples de resposta:
    format('Content-type: application/json~n~n'),
    writeln(Data), % Exibe os dados recebidos no console
    % Aqui você pode validar o movimento e responder à interface gráfica
    format('{"status": "ok", "message": "Movimento realizado com sucesso"}').

% Inicializa o servidor
:- initialization(server(8000)).
