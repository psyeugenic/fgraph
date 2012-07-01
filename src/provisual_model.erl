%% Copyright (C) 2012 Björn-Egil Dahlberg
%%
%% File:    provisual_model.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2012-06-29

-module(provisual_model).

-export([
	new/0,
	vs/1,
	step/1,
	es/1,
	messages/1,
	toggle_edge/1,
	event/2
    ]).

-include_lib("provisual_fgraph.hrl").

-record(model, {
	vs,
	ces,
	es,
	events
    }).


-record(es, {
	links,
	ancestors
    }).

new() ->
    #model{
	vs  = provisual_fgraph:new(),
	ces = ancestors,
	es = #es{
	    links = provisual_fgraph:new(),
	    ancestors = provisual_fgraph:new()
	},
	events = gb_trees:empty()
    }.

step(#model{ events = Es, vs = Vs } = M) ->
    M#model{ 
	vs = provisual_fgraph:step(Vs, es(M), {0.0, 0.0, 0.0}),
	events = decrease_events(Es)
    }.


vs(#model{ vs = Vs}) -> Vs.

es(#model{ ces = links, es = #es{ links = Es}}) -> Es;
es(#model{ ces = ancestors, es = #es{ ancestors = Es}}) -> Es.
messages(#model{ events = Es}) -> gb_trees:to_list(Es).

toggle_edge(#model{ ces = C} = M) -> 
    M#model{ ces = next_edge(C) }.

next_edge(links) -> ancestors;
next_edge(ancestors) -> links.

decrease_events(Es) -> gb_trees:from_orddict(decrease_events(gb_trees:to_list(Es), [])).
decrease_events([{_E,0}|Es], Os) ->
    decrease_events(Es, Os);
decrease_events([{E,I}|Es], Os) ->
    decrease_events(Es, [{E,I-1}|Os]);
decrease_events([], Os) -> Os.

%% graph events

-define(fg_q, 20.0).
-define(fg_m, 0.5).
-define(fg_k, 35.0). % attractive force 
-define(fg_l, 3.0).  % spring length

p1() -> float(random:uniform(160) - 80).
p3() -> {p1(), p1(), p1()}.

event(clear, _) -> provisual_model:new();
event({add_node, Id, Name}, #model{ vs = Vs0} = M) ->
    Vs1 = provisual_fgraph:add(Id, #fg_v{
	    p = p3(),
	    v = {0.0,0.0,0.0},
	    m = ?fg_m,
	    q = ?fg_q,
	    color = undefined,
	    name = Name}, Vs0),
    M#model{vs = Vs1};

event({del_node, Id}, #model{ events = Events, vs = Vs, es = #es{ links = Ls, ancestors = As} = Es} = M) ->
    M#model{ 
	vs = provisual_fgraph:del(Id, Vs),
	events = lists:foldl(fun
		    ({{DId,_},_}, O) when DId =:= Id -> O;
		    ({{_,DId},_}, O) when DId =:= Id -> O;
		    ({Event,I}, O) -> gb_trees:enter(Event,I,O)
		end, gb_trees:empty(), gb_trees:to_list(Events)),
	es = Es#es{
	    links = provisual_fgraph:foldl(fun
		    ({{DId,_},_}, O) when DId =:= Id -> O;
		    ({{_,DId},_}, O) when DId =:= Id -> O;
		    ({K,V}, O) -> provisual_fgraph:add(K,V,O)
		end, provisual_fgraph:new(), Ls),
	    ancestors = provisual_fgraph:foldl(fun
		    ({{DId,_},_}, O) when DId =:= Id -> O;
		    ({{_,DId},_}, O) when DId =:= Id -> O;
		    ({K,V}, O) -> provisual_fgraph:add(K,V,O)
		end, provisual_fgraph:new(), As)
	}
    };

event({add_edge, E, ancestors}, #model{ es = #es{ ancestors = Ls} = Es } = M) ->
    M#model{ 
	es = Es#es{
	    ancestors = provisual_fgraph:add(E, #fg_e{ 
		    k = ?fg_k, 
		    l = ?fg_l
		}, Ls)
	}
    };

event({add_edge, E, links}, #model{ es = #es{ links = Ls} = Es } = M) ->
    M#model{ 
	es = Es#es{
	    links = provisual_fgraph:add(E, #fg_e{ 
		    k = ?fg_k, 
		    l = ?fg_l
		}, Ls)
	}
    };
event({add_event, E, _T}, #model{ events = Events } = M) ->
    M#model{ events = gb_trees:enter(E,100,Events) };

event(Other, M) ->
    io:format("Unhandled graph event ~p ~n", [Other]),
    M.
