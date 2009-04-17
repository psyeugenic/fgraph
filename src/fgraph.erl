-module(fgraph).
-export([
	step_kinetic/2,
	step/2
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

-compile(inline).
-compile({inline_size, 128}).

-include("fgraph.hrl").


%% KEY-VALUE STORAGE dict
%new()             -> dict:new().
%add(K, V, S)      -> dict:store(K, {K,V}, S).
%set(K, V, S)      -> dict:store(K, {K,V}, S).
%del(K, S)         -> dict:erase(K, S).
%get(K, S)         -> case catch dict:fetch(K, S) of {'EXIT',_} -> undefined; {_,Value} -> Value end.
%is_defined(K, S)  -> dict:is_key(K, S).
%size(S)           -> dict:size(S).
%map(F, S)         -> dict:map(fun(_,V) -> F(V) end, S).
%foldl(F, I, S)    -> dict:fold(fun(_, V, O) -> F(V,O) end, I, S).
%foreach(F, S)     -> dict:map(fun(_,V) -> F(V) end, S), S.
%mapfoldl(F, I, S) -> {S, I}.


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


%%% KEY-VALUE STORAGE LIST
%
%%% -> storage()
%
%new() -> [].
%%
%%
%%%% -> true | false
%is_defined(Key, Fg) ->
%    proplists:is_defined(Key, Fg).
%
%size(Fg) -> length(Fg).
%%
%%
%get(Key, Fg) ->
%    proplists:get_value(Key, Fg).
%%
%%
%add(Key, Value, Fg) ->
%    [{Key, Value}|Fg].
%%
%set(Key, Value, Fg) ->
%    [{Key, Value}|proplists:delete(Key, Fg)].
%%
%del(Key, Fg) ->
%    proplists:delete(Key, Fg).
%%
%foreach(Fun, Fg) ->
%    lists:foreach(Fun,Fg).
%%	
%map(Fun, Fg) -> 
%    lists:map(Fun, Fg).
%%
%foldl(Fun, I, Fg) ->
%    lists:foldl(Fun, I, Fg).
%%
%mapfoldl(Fun, I, Fg) ->
%    mapfoldl(Fun, I, Fg).
%%

%% FORCEGRAPH ALGORITHM

resides({X,Y}) ->
    L  = 500,
    Ix = trunc((X + L/2)/L),
    Iy = trunc((Y + L/2)/L),
    {Ix,Iy}.

residents(R) ->
    case get({residents, R}) of
	undefined -> [];
	V -> V
    end.

neighbours({Ix, Iy}) ->
    lists:flatten([
	residents({Ix,Iy}),
	residents({Ix, Iy - 1}),
	residents({Ix + 1, Iy}),
	residents({Ix, Iy + 1}),
	residents({Ix - 1, Iy}),

	residents({Ix - 1, Iy - 1}),
	residents({Ix + 1, Iy - 1}),
	residents({Ix - 1, Iy + 1}),
	residents({Ix + 1, Iy + 1})
     ]).

update_resident(_, {Ix, Iy}, {Ix, Iy}) -> ok;
update_resident(Key, R0, R1) ->
    put({residents, R0}, lists:delete(Key, residents(R0))),
    put({residents, R1}, [Key | residents(R1)]),
    ok.
 
step(Vs, Es) ->
    Pm = {500.0, 500.0},
    fgraph:map(fun
	(Node = {_, #fg_v{ type = static }}) -> Node;
	({Key, Value = #fg_v{ p = {Px, Py}, v = {Vx, Vy}, type = dynamic}}) when is_float(Px), is_float(Py), is_float(Vx), is_float(Vy) ->
	    %F0 = mid_attraction(Key, Value, Pm, {0.0,0.0}), 
	    F0 = {0.0,0.0},
	    F1 = coulomb_repulsion(Key, Value, Vs, F0),
	    F2 = hooke_attraction(Key, Value, Vs, Es, F1),
	    
	    {Fx, Fy} = F2,

	    Vx1 = (Vx + ?fg_th*Fx)*?fg_damp,
	    Vy1 = (Vy + ?fg_th*Fy)*?fg_damp,
    
	    Px1 = Px + ?fg_th*Vx1,
	    Py1 = Py + ?fg_th*Vy1,

    	    {Key, Value#fg_v{ p = {Px1, Py1}, v = {Vx1, Vy1}}};
	(Node) -> Node
    end, Vs).

step_kinetic(Vs, Es) ->
    Pm = {500.0, 500.0},
    fgraph:mapfoldl(fun
	(Node = {_, #fg_v{ type = static }}, KE) -> {Node, KE};
	({Key, Value = #fg_v{ p = {Px, Py}, v = {Vx, Vy}, m = M, type = dynamic}}, KE) when is_float(Px), is_float(Py), is_float(Vx), is_float(Vy) ->
	    F0 = {0.0,0.0},
	    %F0 = mid_attraction(Key, Value, Pm, {0,0}), 
	    F1 = coulomb_repulsion(Key, Value, Vs, F0),
	    F2 = hooke_attraction(Key, Value, Vs, Es, F1),
	    
	    {Fx, Fy} = F2,

	    Vx1 = (Vx + ?fg_th*Fx)*?fg_damp,
	    Vy1 = (Vy + ?fg_th*Fy)*?fg_damp,
    
	    Px1 = Px + ?fg_th*Vx1,
	    Py1 = Py + ?fg_th*Vy1,
	    
	    KE1 = M*math:sqrt(Vx1*Vx1 + Vy1*Vy1)/2 + KE,
    	    {{Key, Value#fg_v{ p = {Px1, Py1}, v = {Vx1, Vy1} }}, KE1};
	(Node, KE) -> {Node, KE}
    end,0, Vs).

step_reduced(Vs, Es) ->
    Pm = {500.0, 500.0},
    fgraph:map(fun
	(Node = {_, #fg_v{ type = static }}) -> Node;
	({Key, Value = #fg_v{ p = {Px, Py}, v = {Vx, Vy}, type = dynamic, resides = undefined}}) when is_float(Px), is_float(Py), is_float(Vx), is_float(Vy) ->
	    F0 = mid_attraction(Key, Value, Pm, {0.0,0.0}), 
	    F1 = coulomb_repulsion(Key, Value, Vs, F0),
	    F2 = hooke_attraction(Key, Value, Vs, Es, F1),
	    
	    {Fx, Fy} = F2,

	    Vx1 = (Vx + ?fg_th*Fx)*?fg_damp,
	    Vy1 = (Vy + ?fg_th*Fy)*?fg_damp,
    
	    Px1 = Px + ?fg_th*Vx1,
	    Py1 = Py + ?fg_th*Vy1,
	    
	    R1  = resides({Px1, Py1}),
	    io:format("greger~n"),
    	    put({residents, R1}, [Key | residents(R1)]),
    	    {Key, Value#fg_v{ p = {Px1, Py1}, v = {Vx1, Vy1}, resides = R1}};
	({Key, Value = #fg_v{ p = {Px, Py}, v = {Vx, Vy}, type = dynamic, resides = R0}}) when is_float(Px), is_float(Py), is_float(Vx), is_float(Vy) ->
	    Vs2 = neighbours(R0),
	    F0 = mid_attraction(Key, Value, Pm, {0.0,0.0}), 
	    F1 = coulomb_repulsion(Key, Value, Vs2, F0),
	    F2 = hooke_attraction(Key, Value, Vs, Es, F1),
	    
	    {Fx, Fy} = F2,

	    Vx1 = (Vx + ?fg_th*Fx)*?fg_damp,
	    Vy1 = (Vy + ?fg_th*Fy)*?fg_damp,
    
	    Px1 = Px + ?fg_th*Vx1,
	    Py1 = Py + ?fg_th*Vy1,

	    R1  = resides({Px1, Py1}),
	    update_resident(Key, R0, R1),
    	    {Key, Value#fg_v{ p = {Px1, Py1}, v = {Vx1, Vy1}, resides = R1}};
	(Node) -> io:format("fallthrouh~n"), Node
    end, Vs).


coulomb_repulsion(K0, #fg_v{ p = P0, q = Q0}, Vs, {Fx0, Fy0}) when is_float(Fx0), is_float(Fy0) ->
    fgraph:foldl(fun
	({K1, _}, F) when K1 == K0 -> F;
	({_, #fg_v{ p = P1, q = Q1}}, {Fx, Fy}) ->
	    {R, {Cx, Cy}} = composition(P0, P1),
	    F = ?fg_kc*(Q1*Q0)/(R*R+0.0001),
	    {Fx + Cx*F, Fy + Cy*F};
	({K1, Value}, F) -> io:format("cr: ~p ~p~n", [K1, Value]), F
    end, {Fx0, Fy0}, Vs).

hooke_attraction(Key0, #fg_v{ p = P0 }, Vs, Es, {Fx0, Fy0}) when is_float(Fx0), is_float(Fy0) ->
    fgraph:foldl(fun
	({{Key1,Key1}, _}, F) -> F;
	({{Key1,Key2}, #fg_e{ l = L, k = K}}, {Fx, Fy}) when Key1 =:= Key0->
	    #fg_v{ p = P1} = fgraph:get(Key2, Vs),
	    {R, {Cx,Cy}} = composition(P0, P1),
	    F = -K*?fg_stretch*(R - L),
	    {Fx + Cx*F, Fy + Cy*F};
	({{Key2,Key1}, #fg_e{ l = L, k = K}}, {Fx, Fy}) when Key1 =:= Key0->
	    #fg_v{ p = P1} = fgraph:get(Key2, Vs),
	    {R, {Cx,Cy}} = composition(P0, P1),
	    F = -K*?fg_stretch*(R - L),
	    {Fx + Cx*F, Fy + Cy*F};
	(_, F) -> F 
    end, {Fx0, Fy0}, Es).
	    

mid_attraction(_Key, #fg_v{ p = P0}, Pm, {Fx0, Fy0}) ->
    K = 30.0,
    {R, {Cx, Cy}} = composition(P0, Pm),
    F = -K*?fg_stretch*(R),
    {Fx0 + Cx*F, Fy0 + F*Cy}.

composition({Px1, Py1}, {Px0, Py0}) when is_float(Px1), is_float(Py1), is_float(Px0), is_float(Py0) ->
    Dx  = Px1 - Px0,
    Dy  = Py1 - Py0,
    R   = math:sqrt(Dx*Dx + Dy*Dy + 0.001),
    {R, {Dx/R, Dy/R}}.
	

rk4(F0) ->
    K1 = ?fg_th*F0,
    K2 = K1 + ?fg_th*F0/2,
    K3 = K2 + ?fg_th*F0/2,
    K4 = K3 + ?fg_th*F0,
    (K1 + K2 + K3 + K4)/6.



