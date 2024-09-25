
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).

:- http_handler('/move', move_handler, []).

% Inicia o servidor na porta 8000
server(Port) :-
    http_server(http_dispatch, [port(Port)]).

% Manipulador de requisições de movimento
move_handler(Request) :-
    http_read_json_dict(Request, Data),
    format('Content-type: application/json~n~n'),
    writeln(Data), % Aqui será inserida a lógica de validação do movimento

    % Enviar resposta para a interface gráfica
    format('{"status": "ok", "message": "Movimento realizado com sucesso"}').

% Inicializa o servidor
:- initialization(server(8000)).
