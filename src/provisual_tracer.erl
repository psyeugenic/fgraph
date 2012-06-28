%% Copyright (C) 2012 Björn-Egil Dahlberg
%%
%% File:    provisual_tracer.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2012-06-15

-module(provisual_tracer).
-export([start/3]).

-record(tstate,{
	n = 1,
	registry,
	procs,
	node,
	client,
	server,
	graph,
	rs}).


start(Node, Graph, Rs) ->
    spawn_link(fun() -> init(Node, Graph, Rs) end).

init(Node, Graph, Rs) ->
    
    Me = self(),
    
    %% setup remote tracer
    FunStr = 
    	"TraceFun = dbg:trace_port(ip, {4799, 16000}), Fun =  fun() -> link(Me), "
	"Port = TraceFun(),"
	"erlang:system_flag(multi_scheduling, block),"
	"Data = [{P, erlang:process_info(P)} || P <- erlang:processes()],"
	"erlang:system_profile(Port, [runnable_procs]),"
	"erlang:trace(all, true, [procs, send, {tracer, Port}]),"
	"Me ! {trace_port, self(), ready},"
	"erlang:system_flag(multi_scheduling, unblock),"
	"Me ! {trace_port, self(), {data, Data}}, receive stop -> stop end end, Fun.",
    
    {_ ,Tokens, _} = erl_scan:string(FunStr),
    {ok, Abs} = erl_parse:parse_exprs(Tokens),
    B = erl_eval:add_binding('Me', Me, erl_eval:new_bindings()),
    {value, SetupFun, _} = erl_eval:exprs(Abs, B),
    RemotePid = rpc:call(Node, erlang, spawn, [SetupFun]),

    % wait for ready then setup trace receiver
    receive {trace_port, RemotePid, ready} -> ok end,

    Handler = fun
        ({drop, N}, In) ->
	    io:format("dropped ~p msg's~n", [N]),
	    In;
    	(Msg, In) ->
	    Me ! Msg,
	    In
    end,
    
    Client = dbg:trace_client(ip, 4799, {Handler, io}),

    %% receive initial data and populate graph

    Pis = receive {trace_port, RemotePid, {data, Data}} -> Data end,

    {Registry, Procs} = apps(Graph, Pis),

    loop(#tstate{
	    registry = Registry, n = 1, rs = Rs, 
	    graph = Graph, node = Node, procs = Procs,
	    client = Client, server = RemotePid
	}).

loop(#tstate{ graph = Graph, n = N} = S) ->
    receive
        stop_tracer ->
	    provisual:clear(Graph),
	    dbg:stop_trace_client(S#tstate.client),
	    S#tstate.server ! stop,
	    ok;

	{profile, _Pid, active, _Mfa, _Ts} ->
	     loop(S#tstate{ n = N + 1});

	{profile, _Pid, inactive, _Mfa, _Ts} ->
	     loop(S#tstate{ n = N - 1});

	{trace, Pid, register, Name} ->
	     loop(S#tstate{ registry = gb_trees:enter(Name, Pid, S#tstate.registry)});
	{trace, _Pid, unregister, _Name} ->
	     loop(S);
         
	 % liveness
	{trace, Pid, exit, _ } ->
	     provisual:del_node(Graph, Pid),
	     loop(S#tstate{ procs = gb_trees:delete(Pid, S#tstate.procs)});
	{trace, Pid, spawn, Pid2, MFAs} ->
	     provisual:add_node(Graph, Pid2, {250, 10, 10}, mfa2name(MFAs)),
	     provisual:add_link(Graph, {Pid, Pid2}, ancestors),
	     loop(S#tstate{ procs = gb_trees:enter(Pid2, ok, S#tstate.procs)});
	
	 % linkage
	{trace, _Pid, getting_unlinked, _Pid2} ->
	     loop(S);
	{trace, _Pid, getting_linked, _Pid2} ->
	     loop(S);
	{trace, Pid, link, Pid2} ->
	     provisual:add_link(Graph, {Pid, Pid2}, links),
	     loop(S);
	{trace, Pid, unlink, Pid2} ->
	     provisual:del_link(Graph, {Pid, Pid2}, links),
	     loop(S);

	 % msgs
	{trace, Pid, send_to_non_existing_process, _Msg, Pid2} ->
	     io:format("Warning: ~p send to non existing process ~p~n", [Pid, Pid2]),
	     loop(S);
	{trace, Pid, send, _, ToPid} ->
	     handle_trace_send(S, Pid, ToPid),
	     loop(S);

	_Other ->	
	    loop(S)
    end.

handle_trace_send(#tstate{ graph = Graph, node = Node, procs = Procs}, Pid, Pid2) when is_pid(Pid2), node(Pid2) =:= Node->
    case gb_trees:lookup(Pid2, Procs) of
	none ->
	    io:format("What the crap: ~p~n", [Pid2]),
	    ok;
	_ ->
    	    provisual:add_event(Graph, {Pid, Pid2}, message)
    end;
handle_trace_send(#tstate{ node = Node } = S, Pid, {Name, Node}) when is_atom(Name) ->
    handle_trace_send(S, Pid, Name);
handle_trace_send(#tstate{ registry = Registry} = S, Pid, Name) when is_atom(Name) ->
    case gb_trees:lookup(Name, Registry) of
	none -> 
	    io:format("Warning: ~p send to non existing atom process ~p~n", [Pid, Name]),
	    ok;
	{value, Pid2} ->
	    handle_trace_send(S, Pid, Pid2)
    end;
handle_trace_send(_, Pid, ToPid) ->
    io:format("Warning: ~p send to unhandled process ~p~n", [Pid, ToPid]),
    ok.


mfa2name({erlang, apply, [M,F,As]}) ->
    mfa2name({M,F,As});
mfa2name({erlang, apply, [Fun, _]}) when is_function(Fun) ->
    {module, M} = erlang:fun_info(Fun, module),
    {name,   N} = erlang:fun_info(Fun, name),
    {arity,  A} = erlang:fun_info(Fun, arity),
    mfa2name({M,N,A});
    
mfa2name({proc_lib, init_p, [_Parent,_Ancestors,Fun]}) when is_function(Fun) ->
    {module, M} = erlang:fun_info(Fun, module),
    {name,   N} = erlang:fun_info(Fun, name),
    {arity,  A} = erlang:fun_info(Fun, arity),
    mfa2name({M,N,A});
mfa2name({proc_lib, init_p, [_Parent,_Ancestors,M,F,As]}) ->
    mfa2name(trans_init(M,F,As));
mfa2name({M, F, Args}) when is_list(Args) -> 
    lists:flatten(io_lib:format("~p:~p/~p", [M,F,length(Args)]));
mfa2name({M, F, A}) -> 
    lists:flatten(io_lib:format("~p:~p/~p", [M,F,A]));
mfa2name(Other) ->
    lists:flatten(io_lib:format("~p", [Other])).

% translate otp application names (as defined in stdlib)
trans_init(gen,init_it,[gen_server,_,_,supervisor,{_,Module,_},_]) -> 
    {supervisor,Module,1};
trans_init(gen,init_it,[gen_server,_,_,_,supervisor,{_,Module,_},_]) ->
    {supervisor,Module,1};
trans_init(gen,init_it,[gen_server,_,_,supervisor_bridge,[Module|_],_]) -> 
    {supervisor_bridge,Module,1};
trans_init(gen,init_it,[gen_server,_,_,_,supervisor_bridge,[Module|_],_]) -> 
    {supervisor_bridge,Module,1};
trans_init(gen,init_it,[gen_server,_,_,Module,_,_]) -> 
    {Module,init,1};
trans_init(gen,init_it,[gen_server,_,_,_,Module|_]) -> 
    {Module,init,1};
trans_init(gen,init_it,[gen_fsm,_,_,Module,_,_]) -> 
    {Module,init,1};
trans_init(gen,init_it,[gen_fsm,_,_,_,Module|_]) ->
    {Module,init,1};
trans_init(gen,init_it,[gen_event|_]) -> 
    {gen_event,init_it,6};
trans_init(M, F, A) when is_atom(M), is_atom(F) -> 
    {M,F,length(A)}.


%% generate pid <-> name registry and populate graph
apps(Graph, Pis) ->
    Registry = lists:foldl(fun
    	({Pid, Pi}, Tree) ->
	    case proplists:get_value(registered_name, Pi) of
	     	undefined -> Tree;
		Name -> gb_trees:enter(Name, Pid, Tree)
	    end
	end, gb_trees:empty(), Pis),
    apps(Graph, Pis, [], Registry, gb_trees:empty(), []).
apps(Graph, [], Parents, Registry, Procs, AllLinks) -> 
    lists:foreach(fun
    	({P1, P2}) ->
	    case gb_trees:is_defined(P1, Procs) of
	    	true ->
		    provisual:add_link(Graph, {P1, P2}, ancestors),
		    ok;
		_ ->
		    ok
	    end
    	end, Parents),
    lists:foreach(fun
        ({Pid, Links}) ->
	    lists:foreach(fun
	    	(Link) when is_pid(Link) ->
	    	    case gb_trees:is_defined(Link, Procs) of
		        true ->
			    % io:format("link ~p~n", [Link]),
			    provisual:add_link(Graph, {Pid, Link}, links);
			false ->
			    ok
		    end;
		(_) -> 
		    ok
	    end, Links)
	end, AllLinks),
    {Registry, Procs};
apps(Graph, [{Pid, Pi}|Pis], Parents, Registry, Procs, AllLinks) ->
    _Status         = proplists:get_value(status, Pi),
    InitialCall    = proplists:get_value(initial_call, Pi),
    RegisteredName = proplists:get_value(registered_name, Pi), 
    Links          = proplists:get_value(links, Pi, []),

    {Name, Relations} = case {InitialCall, RegisteredName} of
	{{proc_lib, init_p, 5}, _} ->
		Dict = proplists:get_value(dictionary, Pi, []),
		Ic   = proplists:get_value('$initial_call', Dict, {proc_lib, init_p, 5}),
		Pl   = case proplists:get_value('$ancestors', Dict) of
		    [ParentAtom|_] when is_atom(ParentAtom) ->
		        case gb_trees:lookup(ParentAtom, Registry) of
			    none ->
				io:format("~p not defined~n", [ParentAtom]),
				[];
			    {value, Parent} ->
		        	[{Parent,Pid}]
			end;
		    [Parent|_] ->
		    	[{Parent,Pid}];
		    _  -> 
		    	[]
		end,
		{mfa2name(Ic), Pl};
	{Mfa, undefined} -> {mfa2name(Mfa), []};
	{_, _Register}    -> {mfa2name(RegisteredName), []}
    end,
    provisual:add_node(Graph, Pid, {250,10,10}, Name),
    apps(Graph, Pis, Relations ++ Parents, Registry, gb_trees:enter(Pid, ok, Procs), [{Pid, Links}|AllLinks]).




