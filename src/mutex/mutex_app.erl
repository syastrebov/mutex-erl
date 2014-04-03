%% Author: djay
%% Created: 16.03.2014
%%
%% Сервер блокировок

-module(mutex_app).
-behaviour(application).

-export([start/2, stop/1]).

-define(DEFAULT_PORT, 7007).

%%--------------------------------------------------------------------
%% Запуск сервера блокировок
%%--------------------------------------------------------------------
start(_StartType, _StartAgrs) ->
    init(client),
    init(mutex).

%%--------------------------------------------------------------------
%% Остановка сервера блокировок
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%--------------------------------------------------------------------
%% Инициализация модулей
%%--------------------------------------------------------------------
init(client) ->
    % Получить слушаемый порт
    Port = case os:getenv("PORT") of
        false    -> ?DEFAULT_PORT;
        ListPort -> list_to_integer(ListPort)
    end,
    io:fwrite("Listening port ~w", [Port]),
    % Открытие сокетного соединения
    case gen_tcp:listen(Port, [binary, {active, true}, {reuseaddr, true}, {nodelay, true}]) of
        {ok, ListenSocket} -> 
            case client_sup:start_link(ListenSocket) of
                {ok, _} -> client_sup:start_child(), true;
                _       -> false
            end;
        Reason ->
            io:fwrite("Unable to start server ~w", [Reason])
    end;
init(mutex) -> mutex_sup:start_link().

