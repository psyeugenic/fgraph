%% Copyright (C) 2012 Björn-Egil Dahlberg
%%
%% File:    provisual.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2012-06-15

-module(provisual).
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
-include_lib("wx/include/gl.hrl").
-include_lib("provisual_fgraph.hrl").

-define(file_connect, 101).
-define(file_disconnect, 102).
-define(view_links, 103).
-define(view_ancestry, 104).
-define(file_cookie, 105).

-compile(inline).

-define(W, 640).
-define(H, 480).

-define(PAN_SPEED, 25).
-define(SHADERS_MENU, 200).
-define(SHADOW_MENU,  201).


-record(cam,
	{origin,
	 distance,     % From origo.
	 azimuth,
	 elevation,
	 pan_x,	       % Panning in X direction.
	 pan_y,	       % Panning in Y direction.
	 fov,	       % Field of view.
	 hither,       % Near clipping plane.
	 yon,	       % Far clipping plane.
	 xs,           % Prev position
	 ys,
	 %% Not camera but needed
	 ww,
	 wh,
	 ortho=false
	}).

-record(s,  {frame, canvas, rstate, font, time, cam, vs, es, es2, sphere}).
-record(rs, {mat_shader = false, shadows = false}).

-record(time, {
	fps=0,      % frames per second
	fc=0,       % frame counter
	diff=0,     % Time last frame in ms
	start =erlang:now(),
	fcst  =erlang:now()}).  % frame counter start time


start() -> spawn(fun() -> init() end).

init() ->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, 1,"Provisual",[{size, {?W,?H}}]),
    wxFrame:connect(Frame, close_window),

    %% Menues
    MenuBar = wxMenuBar:new(),

    File = wxMenu:new([]),
    wxMenu:append(File, ?file_connect, "&Connect"),
    wxMenu:append(File, ?file_disconnect,  "&Disconnect"),
    wxMenu:appendSeparator(File),
    wxMenu:append(File, ?file_cookie, "&Set Cookie"),
    
    View = wxMenu:new([]),
    wxMenu:append(View, ?view_links,  "&Links"),
    wxMenu:append(View, ?view_ancestry, "&Ancestry"),

    wxMenuBar:append(MenuBar, File, "&File"),
    wxMenuBar:append(MenuBar, View, "&View"),
    wxFrame:setMenuBar(Frame, MenuBar),

    wxMenu:connect(Frame, command_menu_selected, []),

    SB = wxFrame:createStatusBar(Frame,[{number,2}]),
    wxStatusBar:setStatusWidths(SB, [-1, 150]),
    SBText = "amera",
    wxFrame:setStatusText(Frame, SBText),

    Attrs = [{attribList, [?WX_GL_RGBA,?WX_GL_DOUBLEBUFFER,0]}],
    Canvas = wxGLCanvas:new(Frame, Attrs),
    wxFrame:connect(Canvas, size),
    wxFrame:connect(Canvas, motion),
    wxFrame:connect(Canvas, left_up),
    wxFrame:connect(Canvas, left_down),
    wxFrame:connect(Canvas, right_down),
    wxFrame:connect(Canvas, mousewheel),

    wxWindow:show(Frame),
    %% Set Current must be called after show and before any opengl call.
    wxGLCanvas:setCurrent(Canvas),

    gl:viewport(0,0,?W,?H),

    gl:enable(?GL_DEPTH_TEST),
    gl:depthFunc(?GL_LEQUAL),
    gl:enable(?GL_CULL_FACE),

    gl:shadeModel(?GL_SMOOTH),
    gl:clearColor(0.1,0.1,0.2,1.0),
    
    DefFont = undefined,
    
    Me = self(),
    register(?MODULE, self()),
    _Ticker = spawn_link(fun() -> ticker_init(Me) end),
    loop(#s{
	    frame=Frame, canvas=Canvas, font=DefFont, 
	    rstate = #rs{}, time=#time{}, cam=camera_init(?W,?H), 
	    vs = vs(), es = es(), es2 = provisual_fgraph:new(),
	    sphere = load_sphere()
	}).


loop(quit) -> ok;
loop(#s{frame=F, es = Es, vs = Vs, 
	sphere = Sphere,canvas=Canvas,
	cam=Cam,time=T,font=_F} = S) ->
    %% Setup camera and light
    io:format("in loop~n"),
    load_matrices(Cam),

    gl:lineWidth(1.0),
    gl:color3f(1.0,1.0,1.0),
    provisual_fgraph:foreach(fun
	    ({{K1,K2}, _}) ->
		#fg_v{p={X1,Y1}} = provisual_fgraph:get(K1, Vs),
		#fg_v{p={X2,Y2}} = provisual_fgraph:get(K2, Vs),
		gl:'begin'(?GL_LINES),
		draw_lines([
			{ X1, 0, Y1},{X2, 0, Y2}
		    ]),
		gl:'end'()
	end, Es),
    gl:color3f(1.0,0.0,0.4),
    provisual_fgraph:foreach(fun
	    ({_Key, #fg_v{ p ={X, Y}}}) ->
		gl:pushMatrix(),
		gl:translatef(X,0.0,Y),
		Sphere(),
		gl:popMatrix()
	end, Vs),

    %% Everything is drawn, show it.
    wxGLCanvas:swapBuffers(Canvas),
    %% Handle events
    Ns = handle_events(S#s{time=fps(F,T)}),
    %% Clear all for the next frame
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),

    %% Sync command queue, so we don't choke the driver and get no events
    _ = wxWindow:getSize(F),
    loop(Ns).


%%%% message handling

handle_events(S) ->
    receive
        Msg -> handle_msg(Msg, S)
    after 0 -> S
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
	[] -> undefined;
	NodeStr ->
	    try
		Node = list_to_atom(NodeStr),
		pong = net_adm:ping(Node),
		provisual_fgraph_win:set_click(Pid, fun
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

handle_msg(#wx{ id=?file_disconnect, event = #wxCommand{type = command_menu_selected}}, #state{ tracer = Tracer} = S) when is_pid(Tracer) ->
    Tracer ! stop_tracer,
    S#state{ tracer = undefined };

handle_msg(#wx{ event = #wxClose{}}, _S) ->
    erlang:halt();

handle_msg(#wx{ id = ?view_links, event = #wxCommand{type = command_menu_selected}}, #state{ pid = _Pid } = S) ->
    S;

handle_msg(#wx{ id = ?view_ancestry, event = #wxCommand{type = command_menu_selected}}, #state{ pid = _Pid} = S) ->
    S;

handle_msg( #wx{event=#wxMouse{ type = right_down}}, S) ->
    S#s{ es = S#s.es2, es2 = S#s.es };

handle_msg( #wx{event=Mouse=#wxMouse{}}, #s{ cam = Cam0}= S) ->
    {_, Cam} = cam_event(Mouse,Cam0),
    S#s{cam=Cam};

handle_msg(#wx{event=#wxSize{size={W,H}}}, #s{ cam = Cam0}= S) ->
    gl:viewport(0,0,W,H),
    S#s{cam=Cam0#cam{ww=W,wh=H}};

handle_msg( {Pid, force_step}, S) ->
    io:format("stepped~n"),
    Vs  = S#s.vs,
    Vs1 = provisual_fgraph:step(Vs, S#s.es, {0.0, 0.0}),
    Pid ! {self(), ok},
    S#s{ vs = Vs1};

handle_msg(Other, S) ->
    io:format("Got ~p ~n",[Other]),
    S.


cam_event(#wxMouse{type=motion, leftDown=false},Cam) ->
    {[], Cam};
cam_event(#wxMouse{type=mousewheel, wheelRotation=Rot},Cam=#cam{distance=Dist}) ->
    case Rot > 0 of
	true ->  {[],Cam#cam{distance=Dist+10}};
	false -> {[],Cam#cam{distance=Dist-10}}
    end;
cam_event(#wxMouse{type=motion, leftDown=true,shiftDown=true,x=X,y=Y},
	  Cam=#cam{pan_x=PanX0,pan_y=PanY0,distance=D,xs=Xs,ys=Ys}) ->
    %% PAN
    S = D*(1/20)/(101-float(?PAN_SPEED)),
    Dx = (X-Xs)*S,
    Dy = (Y-Ys)*S,
    PanX = PanX0 + Dx,
    PanY = PanY0 - Dy,
    {[],Cam#cam{pan_x=PanX,pan_y=PanY,xs=X,ys=Y}};
cam_event(#wxMouse{type=motion, leftDown=true,controlDown=true,x=X, y=Y},
	  Cam=#cam{distance=Dist, ys=Ys}) ->
    %% ZOOM (Dy)
    {[],Cam#cam{distance=Dist+(Y-Ys)/10,xs=X,ys=Y}};
cam_event(#wxMouse{type=motion, leftDown=true,x=X,y=Y},
	  Cam=#cam{azimuth=Az0,elevation=El0,xs=Xs,ys=Ys}) ->
    %% Rotate
    Az = Az0 + (X-Xs),
    El = El0 + (Y-Ys),
    {[], Cam#cam{azimuth=Az,elevation=El, xs=X,ys=Y}};
cam_event(#wxMouse{type=left_down, x=X,y=Y},Cam) ->
    {[],Cam#cam{xs=X,ys=Y}};
cam_event(Ev,Cam) ->
    {Ev,Cam}.


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

    tracer_loop(#tstate{
	    registry = Registry, n = 1, rs = Rs, 
	    graph = Graph, node = Node, procs = Procs,
	    client = Client, server = RemotePid
	}).

tracer_loop(#tstate{ graph = Graph, n = N} = S) ->
    receive
        stop_tracer ->
	    provisual_fgraph_win:clear(Graph),
	    dbg:stop_trace_client(S#tstate.client),
	    S#tstate.server ! stop,
	    ok;

	{profile, _Pid, active, _Mfa, _Ts} ->
	     tracer_loop(S#tstate{ n = N + 1});

	{profile, _Pid, inactive, _Mfa, _Ts} ->
	     tracer_loop(S#tstate{ n = N - 1});

	{trace, Pid, register, Name} ->
	     tracer_loop(S#tstate{ registry = gb_trees:enter(Name, Pid, S#tstate.registry)});
	{trace, _Pid, unregister, _Name} ->
	     tracer_loop(S);
         
	 % liveness
	{trace, Pid, exit, _ } ->
	     provisual_fgraph_win:del_node(Graph, Pid),
	     tracer_loop(S#tstate{ procs = gb_trees:delete(Pid, S#tstate.procs)});
	{trace, Pid, spawn, Pid2, MFAs} ->
	     provisual_fgraph_win:add_node(Graph, Pid2, {250, 10, 10}, mfa2name(MFAs)),
	     provisual_fgraph_win:add_link(Graph, {Pid, Pid2}),
	     tracer_loop(S#tstate{ procs = gb_trees:enter(Pid2, ok, S#tstate.procs)});
	
	 % linkage
	{trace, _Pid, getting_unlinked, _Pid2} ->
	     tracer_loop(S);
	{trace, _Pid, getting_linked, _Pid2} ->
	     tracer_loop(S);
	{trace, Pid, link, Pid2} ->
	     provisual_fgraph_win:add_link(Graph, {Pid, Pid2}, link),
	     tracer_loop(S);
	{trace, Pid, unlink, Pid2} ->
	     provisual_fgraph_win:del_link(Graph, {Pid, Pid2}, link),
	     tracer_loop(S);

	 % msgs
	{trace, Pid, send_to_non_existing_process, _Msg, Pid2} ->
	     io:format("Warning: ~p send to non existing process ~p~n", [Pid, Pid2]),
	     tracer_loop(S);
	{trace, Pid, send, _, ToPid} ->
	     handle_trace_send(S, Pid, ToPid),
	     tracer_loop(S);

	_Other ->	
	    tracer_loop(S)
    end.

handle_trace_send(#tstate{ graph = Graph, node = Node, procs = Procs}, Pid, Pid2) when is_pid(Pid2), node(Pid2) =:= Node->
    case gb_trees:lookup(Pid2, Procs) of
	none ->
	    io:format("What the crap: ~p~n", [Pid2]),
	    ok;
	_ ->
    	    provisual_fgraph_win:add_event(Graph, {Pid, Pid2})
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
		    provisual_fgraph_win:add_link(Graph, {P1, P2}),
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
			    provisual_fgraph_win:add_link(Graph, {Pid, Link}, link);
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
    provisual_fgraph_win:add_node(Graph, Pid, {250,10,10}, Name),
    apps(Graph, Pis, Relations ++ Parents, Registry, gb_trees:enter(Pid, ok, Procs), [{Pid, Links}|AllLinks]).


ticker_init(Pid) -> ticker_loop(Pid, 50).
ticker_loop(Pid, Time) ->
    receive after Time ->
        Pid ! {self(), force_step},
        T0 = now(),
        receive {Pid, ok} -> ok end,
        T1 = now(),
        D = timer:now_diff(T1, T0) div 1000,
        case 40 - D of
            Ms when Ms < 0 -> ticker_loop(Pid, 0);
            Ms -> ticker_loop(Pid, Ms)
        end
    end.



%% gl functions
draw_lines([{X1,Y1,Z1},{X2,Y2,Z2}|Pts]) ->
     gl:vertex3f(X1,Y1,Z1),
     gl:vertex3f(X2,Y2,Z2),
     draw_lines(Pts);
draw_lines([]) ->ok.

load_sphere() ->
    {Size, DataChunk, [Ns]} =
	provisual_sphere:tris([{subd,4}, {ccw,false}, {binary,true},  {scale,4}, {normals,true}]),
    StartNormals = size(DataChunk),
    Data = <<DataChunk/binary, Ns/binary>>,

    [Buff] = gl:genBuffers(1),
    gl:bindBuffer(?GL_ARRAY_BUFFER,Buff),
    gl:bufferData(?GL_ARRAY_BUFFER, size(Data), Data, ?GL_STATIC_DRAW),

    %% Setup color and texture
    gl:color4fv({1.0,1.0,1.0,1.0}),

    %% Setup draw buffers
    gl:bindBuffer(?GL_ARRAY_BUFFER,Buff),

     fun() ->
             %% Draw buffer
             gl:bindBuffer(?GL_ARRAY_BUFFER,Buff),
             gl:vertexPointer(3, ?GL_FLOAT, 0, 0),
             gl:normalPointer(?GL_FLOAT, 0, StartNormals),
             gl:enableClientState(?GL_VERTEX_ARRAY),
             gl:enableClientState(?GL_NORMAL_ARRAY),

             gl:drawArrays(?GL_TRIANGLES, 0, Size*3),

             gl:bindBuffer(?GL_ARRAY_BUFFER, 0),
             gl:disableClientState(?GL_VERTEX_ARRAY),
             gl:disableClientState(?GL_NORMAL_ARRAY)
     end.

fps(Frame, T) ->   fps(Frame, T, 500).
fps(Frame, T0 = #time{fcst=FCSt,start=Start,fc=FC},Interval) ->
    Now = erlang:now(),
    Diff = tdiff(Now,Start),
    Time = tdiff(Now,FCSt),
    if Time > Interval ->
	    Fps = round(1000*FC / Time),
	    wxFrame:setStatusText(Frame, io_lib:format("FPS: ~p",[Fps]),
				  [{number,1}]), %% Zero numbered suddenly
	    T0#time{fc=0,fps=Fps,diff=Diff,fcst=Now};
       true ->
	    T0#time{fc=FC+1,diff=Diff}
    end.

tdiff({A2,B2,C2},{A1,B1,C1}) ->
    (A2-A1)*1000000+(B2-B1)*1000 + (C2-C1) / 1000.


load_matrices(Cam) ->
    load_matrices(Cam,false).
load_matrices(Cam,IncludeLights) ->
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    projection(Cam),
    modelview(Cam, IncludeLights).

projection(#cam{distance=D,fov=Fov,hither=Hither,yon=Yon,
		ww=W,wh=H,ortho=Ortho}) ->
    Aspect = W/H,
    case Ortho of
	false ->
	    glu:perspective(Fov, Aspect, Hither, Yon);
	true ->
	    Sz = D*math:tan(Fov*math:pi()/180/2),
	    gl:ortho(-Sz*Aspect, Sz*Aspect, -Sz, Sz, Hither, Yon)
    end.

modelview(#cam{origin=Origin,distance=Dist,azimuth=Az,
	       elevation=El,pan_x=PanX,pan_y=PanY},
	  Lights) ->
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    if
	is_function(Lights) -> 	Lights();
	true -> ok
    end,
    gl:translatef(PanX, PanY, -Dist),
    gl:rotatef(El, 1, 0, 0),
    gl:rotatef(Az, 0, 1, 0),
    {OX,OY,OZ} = Origin,
    gl:translatef(OX, OY, OZ),
    ok.


%%%%%%%%%%%% Camera

camera_init(W,H) ->
    #cam{origin={0.0,0.0,0.0},
	 azimuth=-45.0,elevation=25.0,
	 distance=50,
	 pan_x=0.0,pan_y=-2.0,
	 fov=45.0,
	 hither=0.1,
	 yon=10000.0,
	 ww=W,
	 wh=H}.
	 
%%%%%%%%%



%% not needed functions

-define(NP, 10).

vs() ->
    Q  = 20.0,   % repulsive force
    M  = 0.5,    % mass
    lists:foldl(fun(K, Vs) ->
		provisual_fgraph:add(K, #fg_v{
			p = {p(), p()},
			m = M,
			q = Q,
			color = undefined,
			name = undefined}, Vs)
	end, provisual_fgraph:new(), lists:seq(1,?NP)).


es() ->
    K   = 30.0,   % attractive force 
    L   =  5.0,   % spring length
    Ids = lists:seq(1,?NP),
    Keys = [{K1,K2}||K1<-Ids,K2<-Ids, K1 =/= K2],
    lists:foldl(fun(Map, Es) ->
		provisual_fgraph:add(Map, #fg_e{ k = K, l = L}, Es)
	end, provisual_fgraph:new(), Keys).


p() -> float(random:uniform(160) - 80).
