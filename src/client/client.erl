%%--------------------------------------------------------------------
%% Процесс обработки команд игрока
%%--------------------------------------------------------------------
-module(client).
-behavior(gen_server).

-include("../../include/client.hrl").

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%--------------------------------------------------------------------
%% Создание экземпляра процесса
%% @param socket ListenSocket
%%--------------------------------------------------------------------
start_link(ListenSocket) ->
    gen_server:start_link(?MODULE, [ListenSocket], []).

%%--------------------------------------------------------------------
%% Инициализация процесса
%% @param socket ListenSocket
%%--------------------------------------------------------------------
init([ListenSocket]) ->
    {ok, #client{listen_socket = ListenSocket}, 0}.

%%--------------------------------------------------------------------
%% Заглушка для событий типа call
%% @param State
%%--------------------------------------------------------------------
handle_call(_, _, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% Отправка сообщения по клиенту tcp
%% 
%% @param binary RawData
%% @param State
%%--------------------------------------------------------------------
handle_cast({gen_tcp, RawData}, State) ->
    case State#client.socket of
        false -> false;
        _     ->
            IsAtom   = is_atom(RawData),
            IsList   = is_list(RawData),
            IsPid    = is_pid(RawData),
            IsTuple  = is_tuple(RawData),
            IsBinary = is_binary(RawData),
            ListData = case true of
                IsAtom   -> atom_to_list(RawData);
                IsPid    -> pid_to_list((RawData));
                IsList   -> RawData;
                IsTuple  -> tuple_to_list(RawData);
                IsBinary -> binary_to_list(RawData);
                _        -> throw(unknown_raw_data)
            end,
            BinaryData = list_to_binary(ListData),
            io:fwrite("Bytes ~p, reciever ~p, data: ~p~n", [byte_size(BinaryData), State#client.socket, BinaryData]),
            gen_tcp:send(State#client.socket, <<BinaryData/binary, <<0>>/binary>>)
    end,
    {noreply, State};

%%--------------------------------------------------------------------
%% Остановка процесса по событию закрытия клиента
%% @param State
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% Инициализация подключения по сокету, установка соединения
%% @param State
%%--------------------------------------------------------------------
handle_info(timeout, State) ->
    #client{listen_socket = ListenSocket} = State,
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            client_sup:start_child(),
            {noreply, State#client{socket = Socket}};
        _Reason ->
            %io:fwrite("Unable to accept tcp connection ~w~n", [_Reason]),
            client_sup:start_child(),
            {stop, normal, State}
    end;

%%--------------------------------------------------------------------
%% Получение входящих пакетов
%% 
%% @param socket Socket
%% @param RawData
%% @param State
%%--------------------------------------------------------------------
handle_info({tcp, _, <<>>}, State) -> {noreply, State};
handle_info({tcp, Socket, RawData}, State) ->
    io:fwrite("input raw data~n"),
    % Объеденяем с пулом предыдущих входящих данных
    StackRawData  = State#client.raw_data,
    ConcatRawData = <<StackRawData/binary, RawData/binary>>,
    io:fwrite("~p~n", [RawData]),
    % Раскодировать входящую команду
    try decode(ConcatRawData) of
        {{object, <<>>, Props}, Rest} ->
            io:format("~w~n", [Props]),
            case call_api(list_to_tuple(lists:sort(Props)), State#client{raw_data = <<>>}) of
                % Вернуть ответ на команду
                {ok, close, Reply, NewState} ->
                    gen_server:cast(self(), {gen_tcp, encode(Reply)}),
                    gen_server:cast(self(), stop),
                    {noreply, NewState};
                % Вернуть ответ на команду
                {ok, Reply, NewState} ->
                    self() ! {tcp, Socket, Rest},
                    gen_server:cast(self(), {gen_tcp, encode(Reply)}),
                    {noreply, NewState};
                % Просто обработать команду
                {noreply, NewState} ->
                    self() ! {tcp, Socket, Rest},
                    {noreply, NewState};
                % Произошла ошибка
                {error, Reason, NewState} ->
                    case Reason of
                        % Завершить процесс
                        disconnect ->
                            {stop, normal, NewState};
                        % Ничего не делать
                        _ ->
                            self() ! {tcp, Socket, Rest},
                            {noreply, NewState}
                    end
            end
    catch
        _ : _X ->
            % Слишком большой объект
            % Сохраняем в стек входящих данных
            io:format("unknown object ~p, reason ~p~n", [RawData, _X]),
            io:fwrite("cant parse object, to stack~n"),
            {noreply, State#client{raw_data = ConcatRawData}}
    end;

%%--------------------------------------------------------------------
%% Закрытие соединения с сокетом, завершения работы процесса
%% @param State
%%--------------------------------------------------------------------
handle_info({tcp_closed, _}, State) ->
    %io:fwrite("connection closed~n"),
    {stop, normal, State};

%%--------------------------------------------------------------------
%% Закрытие соединения с сокетом, завершения работы процесса
%% @param Reason
%% @param State
%%--------------------------------------------------------------------
handle_info({tcp_error, _, Reason}, State) ->
    io:fwrite("connection closed by tcp_error ~p~n", [Reason]),
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% Обработчик остановки процесса
%% При закрытии подключения удаляем все блокировки пользователя
%% 
%% @param State
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    mutex_gs:release_pid(self()),
    io:fwrite("Terminated ~w~n", [self()]),
    ok.

%%--------------------------------------------------------------------
%% Заглушка для горячей замены кода
%%--------------------------------------------------------------------
code_change(_OldVrs, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Декодировать входные данные
%% 
%% @param RawData
%% @return Data
%%--------------------------------------------------------------------
decode(RawData) ->
    case mochijson2:decode(RawData) of
        {struct, [{<<"cmd">>, Cmd}, {<<"name">>, Name}, {<<"timeout">>, Timeout}]} ->
            {{object, <<>>, [{cmd, Cmd}, {name, Name}, {timeout, Timeout}]}, <<>>};
        {struct, [{<<"cmd">>, Cmd}, {<<"name">>, Name}]} ->
            {{object, <<>>, [{cmd, Cmd}, {name, Name}]}, <<>>};
        _ -> throw(cant_parse_raw_data)
    end.

%%--------------------------------------------------------------------
%% Сформировать ответ
%% 
%% @param Data
%% @return RawData
%%--------------------------------------------------------------------
encode(Data) ->
    Data.

%%--------------------------------------------------------------------
%% Вызов обработчика команды
%% 
%% @param object Cmd
%% @param State
%%--------------------------------------------------------------------
call_api(Cmd, State) ->
    case Cmd of
        % Получить указатель на блокировку
        {{cmd, <<"get">>}, {name, Name}, {timeout, Timeout}} ->
            % Останавливаем процесс после отправки сообщения
            {ok, mutex_gs:get(Name, Timeout), State};
        % Установить блокировку
        {{cmd, <<"acquire">>}, {name, Name}} ->
            {ok, mutex_gs:acquire(Name), State};
        % Снять блокировку
        {{cmd, <<"release">>}, {name, Name}} ->
            {ok, mutex_gs:release(Name), State};
        % Неизвестный тип команды
        _ ->
            io:format("unknown input data ~p~n", [Cmd]),
            {error, false, State}
    end.