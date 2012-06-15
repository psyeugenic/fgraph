%% Copyright (C) 2012 Björn-Egil Dahlberg
%%
%% File:    provisual_ticker.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2012-06-15

-module(provisual_ticker).
-export([start/1]).

start(Pid) -> spawn_link(fun() -> init(Pid) end).

init(Pid) -> loop(Pid, 50).
loop(Pid, Time) ->
    receive after Time ->
        Pid ! {self(), force_step},
        T0 = now(),
        receive {Pid, ok} -> ok end, % flow control
        T1 = now(),
        D = timer:now_diff(T1, T0) div 1000,
        case 40 - D of
            Ms when Ms < 0 -> loop(Pid, 0);
            Ms -> loop(Pid, Ms)
        end
    end.
