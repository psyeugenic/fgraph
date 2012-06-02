-module(processes).
-export([start/0]).


-record(state,{
	frame,
	window, 
	pid,
	rs,
	width, 
	height, 
	tracer 
	}).  

-record(tstate,{
	n = 1,
	registry,
	procs,
	node,
	client,
	server,
	graph,
	rs}).


-include_lib("wx/include/wx.hrl").

-define(file_connect, 101).
-define(file_disconnect, 102).
-define(view_links, 103).
-define(view_ancestry, 104).
-define(file_cookie, 105).

start() ->
    spawn(fun() -> init() end).

init() ->
    Wt = 1020,
    Ht = 800,
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, -1, "Processes", []),

    wxFrame:createStatusBar(Frame,[]),
    wxFrame:connect(Frame, close_window),

    MainSz = wxBoxSizer:new(?wxVERTICAL),
    Panel  = wxPanel:new(Frame),
    {Pid,Win} = fgraph_win:new(Panel, []),
    %{Pid2,Win2} = percept_win:new(Panel, []),
    
    %% Menues
    MenuBar = wxMenuBar:new(),

    File    = wxMenu:new([]),
    wxMenu:append(File, ?file_connect, "&Connect"),
    wxMenu:append(File, ?file_disconnect,  "&Disconnect"),
    wxMenu:appendSeparator(File),
    wxMenu:append(File, ?file_cookie, "&Set Cookie"),
    
    View    = wxMenu:new([]),
    wxMenu:append(View, ?view_links,  "&Links"),
    wxMenu:append(View, ?view_ancestry, "&Ancestry"),

    wxMenuBar:append(MenuBar, File, "&File"),
    wxMenuBar:append(MenuBar, View, "&View"),
    wxFrame:setMenuBar(Frame, MenuBar),

    wxMenu:connect(Frame, command_menu_selected, []),

    SF = wxSizerFlags:new(),
    wxSizerFlags:proportion(SF,1),
 
    wxSizer:add(MainSz, Win, wxSizerFlags:proportion(wxSizerFlags:expand(SF),3)),
   
    wxWindow:setSizer(Panel, MainSz),
    wxSizer:fit(MainSz, Frame),
    wxSizer:setSizeHints(MainSz,Frame),
    wxWindow:setSizeHints(Frame, {Wt, Ht}),

    wxWindow:show(Frame),

    loop(#state{ frame = Frame, window = Win, pid = Pid }).


loop(S) ->
    receive
        Msg ->
	    loop(handle_msg(Msg, S))
    end.

handle_msg(#wx{ id=?file_cookie, event = #wxCommand{type = command_menu_selected}, obj = Frame}, S) ->
    TextDialog = wxTextEntryDialog:new(Frame, "Cookie: ", [{caption, "Set Cookie"}]),
    wxDialog:showModal(TextDialog),

    try
    
	case wxTextEntryDialog:getValue(TextDialog) of
	    [] -> ok;
	    CookieStr ->
		Cookie = list_to_atom(CookieStr),
		io:format("Set cookie: ~p ~p~n", [CookieStr, Cookie]),
		erlang:set_cookie(node(), Cookie)
	end
    catch
       C:E ->
       	io:format("crap ~p:~p~n", [C,E])
    end,

    wxDialog:destroy(TextDialog),
    S;

handle_msg(#wx{ id=?file_connect, event = #wxCommand{type = command_menu_selected}, obj = Frame}, #state{ pid = Pid, rs = Rs, tracer = undefined} = S) ->
    TextDialog = wxTextEntryDialog:new(Frame, "Node: ", [{caption, "Connect to Node"}]),
    wxDialog:showModal(TextDialog),

    Tracer = case wxTextEntryDialog:getValue(TextDialog) of
	[] -> 
	    undefined;
	NodeStr ->
	    try
		Node = list_to_atom(NodeStr),
		pong = net_adm:ping(Node),

		fgraph_win:set_click(Pid, fun
		    (Key) -> 
			Pi = rpc:call(Node, erlang, process_info, [Key]),
			[io:format("~p\t~p~n", [K,V]) || {K, V} <- Pi],
			ok
		end),
		
		tracer(Node, Pid, Rs)
	    catch
		C:E ->
		    io:format("connect error ~p:~p~n", [C,E]),
		    undefined
	    end
    end,
    wxDialog:destroy(TextDialog),
    S#state{ tracer = Tracer };

handle_msg(#wx{ id=?file_disconnect, event = #wxCommand{type = command_menu_selected}, obj = Frame}, #state{ pid = Pid, tracer = Tracer} = S) when is_pid(Tracer) ->
    Tracer ! stop_tracer,
    S#state{ tracer = undefined };

handle_msg(#wx{ event = #wxClose{}}, S) ->
    erlang:halt();

handle_msg(#wx{ id = ?view_links, event = #wxCommand{type = command_menu_selected}}, #state{ pid = Pid } = S) ->
    fgraph_win:set_links(Pid, link),
    S;

handle_msg(#wx{ id = ?view_ancestry, event = #wxCommand{type = command_menu_selected}}, #state{ pid = Pid} = S) ->
    fgraph_win:set_links(Pid, default),
    S;

handle_msg(_, S) ->
    S.


tracer(Node, Graph, Rs) ->
    spawn_link(fun() -> tracer_init(Node, Graph, Rs) end).

tracer_init(Node, Graph, Rs) ->
    
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

    tracer_loop(#tstate{ registry = Registry, n = 1, rs = Rs, graph = Graph, node = Node, procs = Procs, client = Client, server = RemotePid}).

tracer_loop(#tstate{ rs = Rs, graph = Graph, n = N, node = Node} = S) ->
    receive
        stop_tracer ->
	    fgraph_win:clear(Graph),
	    dbg:stop_trace_client(S#tstate.client),
	    S#tstate.server ! stop,
	    ok;

	{profile, Pid, active, _Mfa, Ts} ->
	     %fgraph_win:change_node(Graph, Pid, {10,250,10}),
	     %fgraph_win:set_runnability(Graph, N + 1),
	     %percept_win:add(Rs, {Ts, active}),
	     tracer_loop(S#tstate{ n = N + 1});

	{profile, Pid, inactive, _Mfa, Ts} ->
	     %fgraph_win:change_node(Graph, Pid, {250,10,10}),
	     %fgraph_win:set_runnability(Graph, N - 1),
	     %percept_win:add(Rs, {Ts, inactive}),
	     tracer_loop(S#tstate{ n = N - 1});

	{trace, Pid, register, Name} ->
	     % io:format("register ~p ~p~n", [Pid, Name]),
	     tracer_loop(S#tstate{ registry = gb_trees:enter(Name, Pid, S#tstate.registry)});
	{trace, Pid, unregister, _Name} ->
	     tracer_loop(S);
         
	 % liveness
	{trace, Pid, exit, _ } ->
	     fgraph_win:del_node(Graph, Pid),
	     tracer_loop(S#tstate{ procs = gb_trees:delete(Pid, S#tstate.procs)});
	{trace, Pid, spawn, Pid2, MFAs} ->
	 %    fgraph_win:add_node(Graph, Pid),
	     fgraph_win:add_node(Graph, Pid2, {250, 10, 10}, mfa2name(MFAs)),
	     fgraph_win:add_link(Graph, {Pid, Pid2}),
	     tracer_loop(S#tstate{ procs = gb_trees:enter(Pid2, ok, S#tstate.procs)});
	
	 % linkage
	{trace, _Pid, getting_unlinked, _Pid2} ->
	     tracer_loop(S);
	{trace, _Pid, getting_linked, _Pid2} ->
	     tracer_loop(S);
	{trace, Pid, link, Pid2} ->
	     fgraph_win:add_link(Graph, {Pid, Pid2}, link),
	     tracer_loop(S);
	{trace, Pid, unlink, Pid2} ->
	     fgraph_win:del_link(Graph, {Pid, Pid2}, link),
	     tracer_loop(S);

	 % msgs
	{trace, Pid, send_to_non_existing_process, _Msg, Pid2} ->
	     io:format("Warning: ~p send to non existing process ~p~n", [Pid, Pid2]),
	     tracer_loop(S);
	{trace, Pid, send, _, ToPid} ->
	     handle_trace_send(S, Pid, ToPid),
	     tracer_loop(S);

	Other ->	
	    tracer_loop(S)
    end.

handle_trace_send(#tstate{ graph = Graph, node = Node, procs = Procs}, Pid, Pid2) when is_pid(Pid2), node(Pid2) =:= Node->
    case gb_trees:lookup(Pid2, Procs) of
	none ->
	    io:format("What the crap: ~p~n", [Pid2]),
	    ok;
	_ ->
    	    fgraph_win:add_event(Graph, {Pid, Pid2})
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
    
mfa2name({proc_lib, init_p, [Parent,Ancestors,Fun]}) when is_function(Fun) ->
    {module, M} = erlang:fun_info(Fun, module),
    {name,   N} = erlang:fun_info(Fun, name),
    {arity,  A} = erlang:fun_info(Fun, arity),
    mfa2name({M,N,A});
mfa2name({proc_lib, init_p, [Parent,Ancestors,M,F,As]}) ->
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
		    fgraph_win:add_link(Graph, {P1, P2}),
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
			    fgraph_win:add_link(Graph, {Pid, Link}, link);
			false ->
			    ok
		    end;
		(_) -> 
		    ok
	    end, Links)
	end, AllLinks),
    {Registry, Procs};
apps(Graph, [{Pid, Pi}|Pis], Parents, Registry, Procs, AllLinks) ->
    Status         = proplists:get_value(status, Pi),
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
	{_, Register}    -> {mfa2name(RegisteredName), []}
    end,
    fgraph_win:add_node(Graph, Pid, {250,10,10}, Name),
    apps(Graph, Pis, Relations ++ Parents, Registry, gb_trees:enter(Pid, ok, Procs), [{Pid, Links}|AllLinks]).


