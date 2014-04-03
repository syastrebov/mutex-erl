%%--------------------------------------------------------------------
%% Unit test'ы mutex'a
%%--------------------------------------------------------------------

-module(mutex_test).

-include("../include/mutex.hrl").
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% Тестирование очистки занятих блокировок
%%--------------------------------------------------------------------
clean_up_busy_test() ->
    Locks  = dict:new(),
    Locks1 = dict:store('A', #lock{state = ?LOCK_STATE_BUSY, release = common:microtime() - 1}, Locks),
    Locks2 = dict:store('B', #lock{state = ?LOCK_STATE_BUSY, release = common:microtime() - 1}, Locks1),
    Locks3 = dict:store('C', #lock{state = ?LOCK_STATE_FREE, release = common:microtime() - 1}, Locks2),
    Locks4 = dict:store('D', #lock{state = ?LOCK_STATE_BUSY, release = common:microtime() + 1}, Locks3),
    ?assertEqual(2, dict:size(mutex_gs:cleanup_busy(Locks4))).

%%--------------------------------------------------------------------
%% Тестирование очистки свободных блокировок
%%--------------------------------------------------------------------
clean_up_free_test() ->
    Locks  = dict:new(),
    Locks1 = dict:store('A', #lock{state = ?LOCK_STATE_FREE, created = common:microtime() - ?LOCK_MAX_ALIVE_TIMEOUT}, Locks),
    Locks2 = dict:store('B', #lock{state = ?LOCK_STATE_FREE, created = common:microtime() - ?LOCK_MAX_ALIVE_TIMEOUT}, Locks1),
    ?assertEqual(0, dict:size(mutex_gs:cleanup_free(Locks2))).