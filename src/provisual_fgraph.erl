%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%

-module(provisual_fgraph).

-export([
	composition/2,
	load_provisual_nif/0,
	step/3
	]).

-export([
	new/0,
	
	add/3,
	set/3,
	del/2,

	is_defined/2,
	get/2,
	size/1,

	foreach/2,
	map/2,
	foldl/3,
	mapfoldl/3
	]).

-on_load(load_provisual_nif/0).

-compile(inline).
-compile({inline_size, 128}).

-include_lib("provisual_fgraph.hrl").


% size(S)          -> dict:size(S).
% new()            -> dict:new().
% add(K, V, S)     -> dict:store(K, {K,V}, S).
% set(K, V, S)     -> dict:store(K, {K,V}, S).
% del(K, S)        -> dict:erase(K, S).
% get(K, S)        -> case catch dict:fetch(K, S) of 
% 			{'EXIT', _} -> undefined;
% 			{_, V} -> V
% 		    end.
% is_defined(K, S) -> dict:is_key(K, S).
% 
% map(F, S)        -> dict:map(fun(_,V) -> F(V) end, S).
% foldl(F, I, S)   -> dict:fold(fun(_, V, O) -> F(V,O) end, I, S).
% 
% foreach(F, S)    -> dict:map(fun(_,V) -> F(V) end, S), S.
% mapfoldl(_F, I, S) -> {I, S}.


%% KEY-VALUE STORAGE Process dictionary
new() -> [].

is_defined(Key, _Fg) ->
    case get(Key) of
	undefined -> false;
	_ -> true
    end.

get(K, _Fg) ->
    case get(K) of
	{_, V} -> V;
	_ -> undefined
    end.

add(Key, Value, Fg) ->
    put(Key, {Key, Value}),
    [Key|Fg].

set(Key, Value, Fg) ->
    put(Key, {Key, Value}),
    Fg.

size(Fg) -> length(Fg).

del(Key, Fg) ->
    erase(Key),
    lists:delete(Key, Fg).

foreach(Fun, Fg) ->
    lists:foreach(fun
	(Key) -> Fun(get(Key))
    end, Fg),
    Fg.

map(Fun, Fg) -> 
    lists:foreach(fun
	(Key) -> put(Key,Fun(get(Key)))
    end, Fg),
    Fg.

foldl(Fun, I, Fg) ->
    lists:foldl(fun
	(Key, Out) ->
	    Fun(get(Key), Out)
	end, I, Fg).

mapfoldl(Fun, I, Fg) ->
    Acc = lists:foldl(fun
	(Key, Out) ->
	    {Value, Acc} = Fun(get(Key), Out),
	    put(Key, Value),
	    Acc
	end, I, Fg),
    {Fg, Acc}.

step(Vs, Es, Pa) ->
    ?MODULE:map(fun
	(Node = {_, #fg_v{ type = static }}) -> Node;
	({Key, #fg_v{ p = {_,_,_}, type = dynamic} = Value}) ->
	    F0 = {0.0,0.0,0.0},
	    F1 = coulomb_repulsion(Key, Value, Vs, F0),
	    F2 = hooke_attraction(Key, Value, Vs, Es, F1),
	    F3 = point_attraction(Key, Value, Pa, F2),

    	    {Key, force_step(Key, Value, F3)};
	({Key, #fg_v{ p = {_,_}, type = dynamic} = Value}) ->
	    F0 = {0.0,0.0},
	    F1 = coulomb_repulsion(Key, Value, Vs, F0),
	    F2 = hooke_attraction(Key, Value, Vs, Es, F1),
	    F3 = point_attraction(Key, Value, Pa, F2),

    	    {Key, force_step(Key, Value, F3) };
	(Node) -> Node
    end, Vs).

force_step(_Key, #fg_v{ p = {Px, Py, Pz}, v = {Vx, Vy, Vz}} = Value, {Fx, Fy, Fz}) ->
    Vx1 = (Vx + ?fg_th*Fx)*?fg_damp,
    Vy1 = (Vy + ?fg_th*Fy)*?fg_damp,
    Vz1 = (Vz + ?fg_th*Fz)*?fg_damp,

    Px1 = Px + ?fg_th*Vx1,
    Py1 = Py + ?fg_th*Vy1,
    Pz1 = Pz + ?fg_th*Vz1,
    Value#fg_v{ p = {Px1, Py1, Pz1}, v = {Vx1, Vy1, Vz1}};
force_step(_Key, #fg_v{ p = {Px, Py}, v = {Vx, Vy}} = Value, {Fx, Fy}) ->
    Vx1 = (Vx + ?fg_th*Fx)*?fg_damp,
    Vy1 = (Vy + ?fg_th*Fy)*?fg_damp,

    Px1 = Px + ?fg_th*Vx1,
    Py1 = Py + ?fg_th*Vy1,

    Value#fg_v{ p = {Px1, Py1}, v = {Vx1, Vy1}}.

point_attraction(_, #fg_v{ p = P0 }, Pa, {Fx, Fy, Fz}) when is_float(Fx), is_float(Fy), is_float(Fz) ->
    K = 20,
    L = 150,
    {R, {Cx,Cy,Cz}} = ?MODULE:composition(P0, Pa),
    F = -K*?fg_stretch*(R - L),
    {Fx + Cx*F, Fy + Cy*F, Fz + Cz*F};
point_attraction(_, #fg_v{ p = P0 }, Pa, {Fx, Fy}) when is_float(Fx), is_float(Fy) ->
    K = 20,
    L = 150,
    {R, {Cx,Cy}} = ?MODULE:composition(P0, Pa),
    F = -K*?fg_stretch*(R - L),
    {Fx + Cx*F, Fy + Cy*F}.

coulomb_repulsion(K0, #fg_v{ p = P0, q = Q0}, Vs, {Fx0, Fy0,Fz0}) when is_float(Fx0), is_float(Fy0), is_float(Fz0) ->
    ?MODULE:foldl(fun
	({K1, _}, F) when K1 == K0 -> F;
	({_, #fg_v{ p = P1, q = Q1}}, {Fx, Fy, Fz}) ->
	    {R, {Cx, Cy,Cz}} = ?MODULE:composition(P0, P1),
	    F = ?fg_kc*(Q1*Q0)/(R*R+?fg_sqrt_eps),
	    {Fx + Cx*F, Fy + Cy*F, Fz + Cz*F};
	(_, F) -> F
    end, {Fx0, Fy0, Fz0}, Vs);
coulomb_repulsion(K0, #fg_v{ p = P0, q = Q0}, Vs, {Fx0, Fy0}) when is_float(Fx0), is_float(Fy0) ->
    ?MODULE:foldl(fun
	({K1, _}, F) when K1 == K0 -> F;
	({_, #fg_v{ p = P1, q = Q1}}, {Fx, Fy}) ->
	    {R, {Cx, Cy}} = ?MODULE:composition(P0, P1),
	    F = ?fg_kc*(Q1*Q0)/(R*R+?fg_sqrt_eps),
	    {Fx + Cx*F, Fy + Cy*F};
	(_, F) -> F
    end, {Fx0, Fy0}, Vs).

hooke_attraction(Key0, #fg_v{ p = P0 }, Vs, Es, {Fx0, Fy0, Fz0}=F0) when is_float(Fx0), is_float(Fy0), is_float(Fz0) ->
    ?MODULE:foldl(fun
	({{Key1,Key1}, _}, F) -> F;
	({{Key1,Key2}, #fg_e{ l = L, k = K}}, {Fx, Fy, Fz}) when Key1 =:= Key0->
	    try
	    #fg_v{ p = P1} = ?MODULE:get(Key2, Vs),
	    {R, {Cx,Cy,Cz}} = ?MODULE:composition(P0, P1),
	    F = -K*?fg_stretch*(R - L),
	    {Fx + Cx*F, Fy + Cy*F, Fz + Cz*F}
	    catch
		C:E ->
		   io:format("{~p,~p} : bad key ~p ~n", [C,E,Key2]),
		   {Fx, Fy, Fz}
	    end;
	({{Key2,Key1}, #fg_e{ l = L, k = K}}, {Fx, Fy, Fz}) when Key1 =:= Key0->
	    try
	    #fg_v{ p = P1} = ?MODULE:get(Key2, Vs),
	    {R, {Cx,Cy,Cz}} = ?MODULE:composition(P0, P1),
	    F = -K*?fg_stretch*(R - L),
	    {Fx + Cx*F, Fy + Cy*F, Fz + Cz*F}
	    catch
		C:E ->
		   io:format("{~p,~p} : bad key ~p ~n", [C,E,Key2]),
		   {Fx, Fy, Fz}
	    end;
	(_, F) -> F 
    end, F0, Es);
hooke_attraction(Key0, #fg_v{ p = P0 }, Vs, Es, {Fx0, Fy0}) when is_float(Fx0), is_float(Fy0) ->
    ?MODULE:foldl(fun
	({{Key1,Key1}, _}, F) -> F;
	({{Key1,Key2}, #fg_e{ l = L, k = K}}, {Fx, Fy}) when Key1 =:= Key0->
	    try
	    #fg_v{ p = P1} = ?MODULE:get(Key2, Vs),
	    {R, {Cx,Cy}} = ?MODULE:composition(P0, P1),
	    F = -K*?fg_stretch*(R - L),
	    {Fx + Cx*F, Fy + Cy*F}
	    catch
		C:E ->
		   io:format("{~p,~p} : bad key ~p ~n", [C,E,Key2]),
		   {Fx, Fy}
	    end;
	({{Key2,Key1}, #fg_e{ l = L, k = K}}, {Fx, Fy}) when Key1 =:= Key0->
	    try
	    #fg_v{ p = P1} = ?MODULE:get(Key2, Vs),
	    {R, {Cx,Cy}} = ?MODULE:composition(P0, P1),
	    F = -K*?fg_stretch*(R - L),
	    {Fx + Cx*F, Fy + Cy*F}
	    catch
		C:E ->
		   io:format("{~p,~p} : bad key ~p ~n", [C,E,Key2]),
		   {Fx, Fy}
	    end;
	(_, F) -> F 
    end, {Fx0, Fy0}, Es).

% This decomposition takes a lot of computing power, needs optimizing
composition({Px1, Py1, Pz1}, {Px0, Py0, Pz0}) when is_float(Px1), is_float(Py1), is_float(Pz1),
						   is_float(Px0), is_float(Py0), is_float(Pz0) ->
    Dx = Px1 - Px0,
    Dy = Py1 - Py0,
    Dz = Pz1 - Pz0,
    R  = qsqrt(Dx*Dx + Dy*Dy + Dz*Dz + ?fg_sqrt_eps),
    %R  = math:sqrt(Dx*Dx + Dy*Dy + Dz*Dz + ?fg_sqrt_eps),
    {R, {Dx/R, Dy/R, Dz/R}};
composition({Px1, Py1}, {Px0, Py0}) when is_float(Px1), is_float(Py1), is_float(Px0), is_float(Py0) ->
    Dx = Px1 - Px0,
    Dy = Py1 - Py0,
    R  = qsqrt(Dx*Dx + Dy*Dy + ?fg_sqrt_eps),
    %R  = math:sqrt(Dx*Dx + Dy*Dy + ?fg_sqrt_eps),
    {R, {Dx/R, Dy/R}}.

%% Carmacks Quake3 square root trick (sadly not faster then builtin math:sqrt/1)
qsqrt(X) ->
    X2 = X*0.5,
    <<I:32>> = <<X:32/float>>,
    Trick = 16#5f3759df - ( I bsr 1 ),
    <<Inv:32/float>> = <<Trick:32>>,
    Inv0 = Inv*(1.50 - (X2 * Inv * Inv)),
    % Inv1 = Inv0*(1.5 - (X2 * Inv0 * Inv0)),
    1/Inv0.

load_provisual_nif() ->
    case code:priv_dir(provisual) of
        {error, bad_name} ->
            SoName = filename:join("priv", provisual_drv);
        Dir ->
            SoName = filename:join(Dir, provisual_drv)
    end,
    erlang:load_nif(SoName, 0).


