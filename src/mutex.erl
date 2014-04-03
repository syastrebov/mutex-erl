%% Author: djay
%% Created: 16.03.2014
%% Description: TODO: Add description to mutex
-module(mutex).
-export([start/0]).

start() ->
    application:start(mutex).