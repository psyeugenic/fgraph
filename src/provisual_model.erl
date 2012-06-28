%% Copyright (C) 2012 Björn-Egil Dahlberg
%%
%% File:    provisual_model.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2012-06-29

-module(provisual_model).

-export([
	new/0,
	vs/1,
	set_vs/2,
	es/1,
	toggle_edge/1,
	graph_event/2
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
	events = []
    }.

vs(#model{ vs = Vs}) -> 
    Vs.

set_vs(#model{} = M, Vs) ->
    M#model{ vs = Vs }.

es(#model{ ces = links, es = #es{ links = Es}}) -> Es;
es(#model{ ces = ancestors, es = #es{ ancestors = Es}}) -> Es.

toggle_edge(#model{ ces = C} = M) -> 
    M#model{ ces = next_edge(C) }.

next_edge(links) -> ancestors;
next_edge(ancestors) -> links.

%% graph events

-define(fg_q, 20.0).
-define(fg_m, 0.5).
-define(fg_k, 30.0). % attractive force 
-define(fg_l, 5.0).  % spring length

p1() -> float(random:uniform(160) - 80).
p3() -> {p1(), p1(), p1()}.

graph_event(clear, _) -> provisual_model:new();
graph_event({add_node, Id, Name}, #model{ vs = Vs0} = M) ->
    Vs1 = provisual_fgraph:add(Id, #fg_v{
	    p = p3(),
	    v = {0.0,0.0,0.0},
	    m = ?fg_m,
	    q = ?fg_q,
	    color = undefined,
	    name = Name}, Vs0),
    M#model{vs = Vs1};

graph_event({del_node, Id}, #model{ vs = Vs, es = #es{ links = Ls, ancestors = As} = Es} = M) ->
    M#model{ 
	vs = provisual_fgraph:del(Id, Vs),
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

graph_event({add_edge, E, ancestors}, #model{ es = #es{ ancestors = Ls} = Es } = M) ->
    M#model{ 
	es = Es#es{
	    ancestors = provisual_fgraph:add(E, #fg_e{ 
		    k = ?fg_k, 
		    l = ?fg_l
		}, Ls)
	}
    };

graph_event({add_edge, E, links}, #model{ es = #es{ links = Ls} = Es } = M) ->
    M#model{ 
	es = Es#es{
	    links = provisual_fgraph:add(E, #fg_e{ 
		    k = ?fg_k, 
		    l = ?fg_l
		}, Ls)
	}
    };
graph_event(Other, M) ->
    io:format("Unhandled graph event ~p ~n", [Other]),
    M.
