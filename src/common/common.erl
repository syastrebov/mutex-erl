%%--------------------------------------------------------------------
%% Базовые функции
%%--------------------------------------------------------------------
-module(common).

-export([timestamp/0, timestamp/1, microtime/0, datetime_to_now/1]).
-export([output/1, flag/3, md5_hex/1, divider/1, safe_list_to_integer/1, random/1, count_with_description/4]).
-export([floor/1, ceiling/1]).
-export([convert_bin_to_uid/1, convert_uid_to_bin/1]).

%%--------------------------------------------------------------------
%% Запись в файл
%% @oaram list Data
%%--------------------------------------------------------------------
output(Data) ->
    {ok, WriteDescr} = file:open("output.dat", [raw, append]),
    file:write(WriteDescr, Data++"~n"),
    file:close(WriteDescr).

%%--------------------------------------------------------------------
%% Получение текущего времени в формате timestamp
%% @return int
%%--------------------------------------------------------------------
timestamp() ->
    timer:now_diff(now(), {0,0,0}) div 1000000.

%%--------------------------------------------------------------------
%% Получение текущего времени в формате timestamp
%% @param datetime DateTime
%% 
%% @return int
%%--------------------------------------------------------------------
timestamp(DateTime) ->
    timer:now_diff(datetime_to_now(DateTime), {0,0,0}) div 1000000.

%%--------------------------------------------------------------------
%% Конвертировать дату в timestamp
%% @param datetime DateTime
%% 
%% @return timestamp
%%--------------------------------------------------------------------
datetime_to_now(DateTime) ->
    [DateTime1|_] = calendar:local_time_to_universal_time_dst(DateTime),
    Seconds = calendar:datetime_to_gregorian_seconds(DateTime1) - 62167219200,
    %% 62167219200 == calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}})
    {Seconds div 1000000, Seconds rem 1000000, 0}.

%%--------------------------------------------------------------------
%% Получение текущего времени в формате microtime
%% @return int
%%--------------------------------------------------------------------
microtime() ->
    timer:now_diff(now(), {0,0,0}) div 1000.

%%--------------------------------------------------------------------
%% Управление бинарной маской
%% @param int Flags
%% @param int Flag
%%--------------------------------------------------------------------
flag(check,   Flags, Flag) when Flags band Flag > 0  -> true;
flag(not_set, Flags, Flag) when Flags band Flag == 0 -> true;
flag(set,     Flags, Flag) -> Flags bor Flag;
flag(unset,   Flags, Flag) -> Flags band bnot Flag;
flag(_, _, _) -> false.

%%--------------------------------------------------------------------
%% Получить md5 строки
%% @param mixed S
%%
%% @return list
%%--------------------------------------------------------------------
md5_hex(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).

%%--------------------------------------------------------------------
%% Возвращает 1 если 0
%%--------------------------------------------------------------------
divider(N) when is_integer(N) == false -> 1;
divider(N) when N == 0 -> 1;
divider(N) -> N.

%%--------------------------------------------------------------------
%% Преобразование list в int без сообщения об ошибке
%% @param list List
%% 
%% @return int or false
%%--------------------------------------------------------------------
safe_list_to_integer(List) ->
    try list_to_integer(List) of
        Integer -> Integer
    catch 
        _:_ -> false
    end.

%%--------------------------------------------------------------------
%% Конвертация бинарных данных в uid
%% @param BinUid
%%
%% @return list or integer
%%--------------------------------------------------------------------
convert_bin_to_uid(BinUid) when is_integer(BinUid) == true -> integer_to_list(BinUid);
convert_bin_to_uid(BinUid) when is_list(BinUid) == true -> BinUid;
convert_bin_to_uid(BinUid) -> binary_to_list(BinUid).

%%--------------------------------------------------------------------
%% Конвертация uid в бинарные данные
%% @param Uid
%%
%% @return binary or integer
%%--------------------------------------------------------------------
convert_uid_to_bin(Uid) when is_integer(Uid) == true -> list_to_binary(integer_to_list(Uid));
convert_uid_to_bin(Uid) when is_list(Uid) -> list_to_binary(Uid);
convert_uid_to_bin(Uid) -> Uid.

%%--------------------------------------------------------------------
%% Рандомное число от 1 до N
%% @param integer N
%%
%% @return integer
%%--------------------------------------------------------------------
random(N) when N < 2 -> 1;
random(N) -> crypto:rand_uniform(1, N + 1).

%%--------------------------------------------------------------------
%% Склонение числа в зависимости от количества
%% 
%% @param integer N
%% @param list Word1
%% @param list Word2
%% @param list Word0
%%
%% @return list
%%--------------------------------------------------------------------
count_with_description(Count, Word1, Word2, Word0) ->
    Modulo100 = abs(Count rem 100),
    Modulo10  = abs(Count rem 10),
    % Проверки для определения склонения
    Word1Check = Modulo10 == 1 andalso Modulo100 =/= 11,
    Word2Check = lists:member(Modulo10, [2, 3, 4]) andalso lists:member(Modulo100, [12, 13, 14]) == false,
    % Выбор соотвествующего слова
    if
        Word1Check -> integer_to_list(Count) ++ Word1;
        Word2Check -> integer_to_list(Count) ++ Word2;
        true       -> integer_to_list(Count) ++ Word0
    end.

%%--------------------------------------------------------------------
%% Округление в меньшую стороную
%% @param number X
%% 
%% @return integer
%%--------------------------------------------------------------------
floor(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T - 1;
        Pos when Pos > 0 -> T;
        _ -> T
    end.

%%--------------------------------------------------------------------
%% Округление в большую стороную
%% @param number X
%% 
%% @return integer
%%--------------------------------------------------------------------
ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.