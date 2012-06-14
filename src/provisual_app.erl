%% Copyright (C) 2012 Björn-Egil Dahlberg
%%
%% File:    provisual_app.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2012-06-15

-module(provisual_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    provisual_sup:start_link().

stop(_State) ->
    ok.
