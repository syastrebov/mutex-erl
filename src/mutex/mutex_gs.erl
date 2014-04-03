%% Author: djay
%% Created: 16.03.2014
%% Description: gen server для обработки очереди блокировок

-module(mutex_gs).
-behaviour(gen_server).

-include("../../include/mutex.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([start_link/0, get/2, acquire/1, release/1, release_pid/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-ifdef(TEST).
-export([cleanup_busy/1, cleanup_free/1]).
-endif.

%%--------------------------------------------------------------------
%% Создание экземпляра процесса
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% Инициализация процесса
%%--------------------------------------------------------------------
init(_Args) ->
    erlang:send_after(?LOCK_CLEANUP_TIMEOUT, self(), cleanup),
    {ok, #mutex{debug_mode = true}}.

%%--------------------------------------------------------------------
%% Получить указатель на блокировку
%%
%% @param list Name
%% @param int Timeout
%%--------------------------------------------------------------------
get(Name, Timeout) ->
    gen_server:call(?MODULE, {get, Name, Timeout}).

%%--------------------------------------------------------------------
%% Установить блокировку
%% 
%% @param list Name
%%--------------------------------------------------------------------
acquire(Name) ->
    gen_server:call(?MODULE, {acquire, Name}).

%%--------------------------------------------------------------------
%% Снять блокировку
%% 
%% @param list Name
%%--------------------------------------------------------------------
release(Name) ->
    gen_server:call(?MODULE, {release, Name}).

%%--------------------------------------------------------------------
%% Снять все блокировки пользователя
%% 
%% @param list Name
%%--------------------------------------------------------------------
release_pid(Pid) ->
    gen_server:cast(?MODULE, {release, Pid}).

%%--------------------------------------------------------------------
%% Получить ссылку на блокировку
%% 
%% @param Name
%% @param Timeout
%% @param State
%%--------------------------------------------------------------------
handle_call({get, Name, Timeout}, {Pid, _}, State) ->
    {reply, Name, State#mutex{locks = dict:store({Pid, Name}, #lock{
        pid     = Pid, 
        name    = Name, 
        timeout = Timeout,
        state   = ?LOCK_STATE_FREE,
        created = common:microtime()
    }, State#mutex.locks)}};

%%--------------------------------------------------------------------
%% Установить блокировку
%% 
%% @param Pid
%% @param State
%%--------------------------------------------------------------------
handle_call({acquire, Name}, {Pid, _}, State) ->
    % Находим блокировку по pid
    case dict:find({Pid, Name}, State#mutex.locks) of
        error         -> {reply, not_found, State};
        {ok, Current} ->
            % Проверяем, что еще не занята
            case Current#lock.state of
                ?LOCK_STATE_BUSY -> {reply, already_acquired, State};
                ?LOCK_STATE_FREE ->
                    % Проверяем есть ли занятые блокировки по ключу
                    Locked = dict:size(dict:filter(fun(_, Lock) -> 
                        Lock#lock.name == Current#lock.name 
                            andalso Lock#lock.state == ?LOCK_STATE_BUSY 
                            andalso Lock#lock.release > common:microtime()
                    end, State#mutex.locks)),
                    if
                        Locked > 0 -> {reply, busy, State};
                        true       -> 
                            % Определяем время авторазблокировки
                            Release = case Current#lock.timeout of
                                false   -> common:microtime() + ?LOCK_MAX_TIMEOUT;
                                Timeout -> common:microtime() + Timeout
                            end,
                            debug_msg("common:microtime = ~p release = ~p~n", 
                                      [common:microtime(), Current#lock.timeout], State),
                            % Блокировка успешно занята
                            {reply, acquired, State#mutex{locks = dict:store({Pid, Name}, Current#lock{
                                state   = ?LOCK_STATE_BUSY, 
                                release = Release
                            }, State#mutex.locks)}}
                    end
            end
    end;

%%--------------------------------------------------------------------
%% Снять блокировку
%% Находим блокировку по pid и удаляем
%% 
%% @param Pid
%% @param State
%%--------------------------------------------------------------------
handle_call({release, Name}, {Pid, _}, State) ->
    case dict:find({Pid, Name}, State#mutex.locks) of
        error   -> {reply, not_found, State};
        {ok, _} -> {reply, released, State#mutex{locks = dict:erase({Pid, Name}, State#mutex.locks)}}
    end.

%%--------------------------------------------------------------------
%% Снять все блокировки пользователя при отключении пользователя
%%
%% @param pid Pid
%% @param State
%%--------------------------------------------------------------------
handle_cast({release, Pid}, State) ->
    debug_msg("release all ~p, in pool ~p~n", [Pid, dict:size(State#mutex.locks)], State),
    ClearPid = dict:filter(fun(_, Lock) -> Lock#lock.pid =/= Pid end, State#mutex.locks),
    debug_msg("Cleanup, in pool left ~p~n", [dict:size(ClearPid)], State),
    {noreply, State#mutex{locks = ClearPid}};

%%--------------------------------------------------------------------
%% Остановить процесс
%% @param State
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% Очистка протухших блокировок
%% @param State
%%--------------------------------------------------------------------
handle_info(cleanup, State) ->
    % Очищаем протухшие активные блокировки
    CleanupBusy = cleanup_busy(State#mutex.locks),
    % Ощищаем протухшие созданные блокировки
    CleanupFree = cleanup_free(CleanupBusy),
    debug_msg("Cleanup, in pool left ~w~n", [dict:size(CleanupFree)], State),
    % Повторяем через интервал
    erlang:send_after(?LOCK_CLEANUP_TIMEOUT, self(), cleanup),
    {noreply, State#mutex{locks = CleanupFree}}.

%%--------------------------------------------------------------------
%% Обработчик остановки процесса
%% @param State
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Заглушка для горячей замены кода
%%--------------------------------------------------------------------
code_change(_OldVrs, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Очистка протухших занятых блокировок
%% 
%% @param dict Locks
%% @return dict
%%--------------------------------------------------------------------
cleanup_busy(Locks) ->
    dict:filter(fun(_, Lock) -> 
        (Lock#lock.state == ?LOCK_STATE_BUSY andalso Lock#lock.release < common:microtime()) == false 
    end, Locks).

%%--------------------------------------------------------------------
%% Очистка протухших свободных блокировок
%% 
%% @param dict Locks
%% @return dict
%%--------------------------------------------------------------------
cleanup_free(Locks) ->
    dict:filter(fun(_, Lock) ->
        MaxLiveTime = Lock#lock.created + ?LOCK_MAX_ALIVE_TIMEOUT,
        (Lock#lock.state == ?LOCK_STATE_FREE andalso MaxLiveTime =< common:microtime()) == false
    end, Locks).

%%--------------------------------------------------------------------
%% Режим отладки
%% 
%% @param list  MsgTemplate
%% @param list  Args
%% @param mutex State
%%--------------------------------------------------------------------
debug_msg(MsgTemplate, Args, State) when State#mutex.debug_mode == true ->
    io:fwrite(MsgTemplate, Args);
debug_msg(_, _, _) ->
    debug_mode_disabled.