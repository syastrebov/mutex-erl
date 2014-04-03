%% Author: djay
%% Created: 16.03.2014
%% Description: TODO: Add description to my_mutex_sup

-module(mutex_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% Создание экземпляра процесса
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%--------------------------------------------------------------------
%% Инициализация супервизора
%%--------------------------------------------------------------------
init([]) ->
    {ok, {{one_for_one, 5, 10}, [{mutex_gs, {mutex_gs, start_link, []},  permanent, 2000, worker, [mutex_gs]}]}}.