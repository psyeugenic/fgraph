%% Copyright (C) 2012 Björn-Egil Dahlberg
%%
%% File:    provisual.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2012-06-15

-module(provisual).
-export([start/0]).

-export([
	add_node/4,
	del_node/2,
	add_link/3,
	del_link/3,
	add_event/3,
	clear/1
    ]).



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

-record(s,  {
	frame, 
	canvas, 
	rstate, 
	font, 
	time, 
	cam, 
	vs, 
	es, 
	es2, 
	sphere,
	tracer
    }).

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
    _Ticker = provisual_ticker:start(Me),
    loop(#s{
	    frame=Frame, canvas=Canvas, font=DefFont, 
	    rstate = #rs{}, time=#time{}, cam=camera_init(?W,?H), 
	    vs  = provisual_fgraph:new(),
	    es  = provisual_fgraph:new(),
	    es2 = provisual_fgraph:new(),
	    sphere = load_sphere()
	}).


loop(quit) -> ok;
loop(#s{frame=F, es = Es, vs = Vs, 
	sphere = Sphere,canvas=Canvas,
	cam=Cam,time=T,font=_F} = S) ->
    %% Setup camera and light
    load_matrices(Cam),

    gl:lineWidth(1.0),
    gl:color3f(1.0,1.0,1.0),
    provisual_fgraph:foreach(fun
	    ({{K1,K2}, _}) ->
		#fg_v{p={X1,Y1,Z1}} = provisual_fgraph:get(K1, Vs),
		#fg_v{p={X2,Y2,Z2}} = provisual_fgraph:get(K2, Vs),
		gl:'begin'(?GL_LINES),
		draw_lines([
			{ X1, Z1, Y1},{X2, Z2, Y2}
		    ]),
		gl:'end'()
	end, Es),
    gl:color3f(1.0,0.0,0.4),
    provisual_fgraph:foreach(fun
	    ({_Key, #fg_v{ p ={X, Y, Z}}}) ->
		gl:pushMatrix(),
		gl:translatef(X,Z,Y),
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

handle_msg(#wx{ id=?file_connect, event = #wxCommand{type = command_menu_selected}, obj = Frame}, #s{ tracer = undefined} = S) ->
    TextDialog = wxTextEntryDialog:new(Frame, "Node: ", [{caption, "Connect to Node"}]),
    wxDialog:showModal(TextDialog),

    Tracer = case wxTextEntryDialog:getValue(TextDialog) of
	[] -> undefined;
	NodeStr ->
	    io:format("connect to node: ~p~n", [NodeStr]),
	    try
		Node = list_to_atom(NodeStr),
		pong = net_adm:ping(Node),
	%	provisual_fgraph:set_click(Pid, fun
	%	    (Key) -> 
	%		Pi = rpc:call(Node, erlang, process_info, [Key]),
	%		[io:format("~p\t~p~n", [K,V]) || {K, V} <- Pi],
	%		ok
	%	end),
		
	provisual_tracer:start(Node, self(), [])
	    catch
		C:E ->
		    io:format("connect error ~p:~p~n", [C,E]),
		    undefined
	    end
    end,
    wxDialog:destroy(TextDialog),
    S#s{ tracer = Tracer };

handle_msg(#wx{ id=?file_disconnect, event = #wxCommand{type = command_menu_selected}}, #s{ tracer = Tracer} = S) when is_pid(Tracer) ->
    Tracer ! stop_tracer,
    S#s{ tracer = undefined };

handle_msg(#wx{ event = #wxClose{}}, _S) ->
    erlang:halt();

handle_msg(#wx{ id = ?view_links, event = #wxCommand{type = command_menu_selected}}, #s{ } = S) ->
    S;

handle_msg(#wx{ id = ?view_ancestry, event = #wxCommand{type = command_menu_selected}}, #s{ } = S) ->
    S;

handle_msg( #wx{event=#wxMouse{ type = right_down}}, S) ->
    S#s{ es = S#s.es2, es2 = S#s.es };

handle_msg( #wx{event=Mouse=#wxMouse{}}, #s{ cam = Cam0}= S) ->
    {_, Cam} = cam_event(Mouse,Cam0),
    S#s{cam=Cam};

handle_msg(#wx{event=#wxSize{size={W,H}}}, #s{ cam = Cam0}= S) ->
    gl:viewport(0,0,W,H),
    S#s{cam=Cam0#cam{ww=W,wh=H}};

handle_msg({Pid, force_step}, S) ->
    Vs  = S#s.vs,
    Vs1 = provisual_fgraph:step(Vs, S#s.es, {0.0, 0.0, 0.0}),
    Pid ! {self(), ok},
    S#s{ vs = Vs1};

handle_msg({graph, Cmd}, S) ->
    graph_event(Cmd, S);

handle_msg(#wx{ id=Id, event = #wxCommand{type = command_menu_selected}}, S) ->
    io:format("wx Id = ~p~n", [Id]),
    S;
handle_msg(Other, S) ->
    io:format("Got ~p ~n",[Other]),
    S.

graph_event(clear, S) -> 
    S#s{ vs = provisual_fgraph:new(), es = provisual_fgraph:new(), es2 = provisual_fgraph:new()};
graph_event({add_node, Id, Name}, #s{ vs = Vs0} = S) ->
    Q  = 20.0,   % repulsive force
    M  = 0.5,    % mass
    Vs1 = provisual_fgraph:add(Id, #fg_v{
	    p = p3(),
	    v = {0.0,0.0,0.0},
	    m = M,
	    q = Q,
	    color = undefined,
	    name = Name}, Vs0),
    S#s{vs = Vs1};
graph_event({del_node, Id}, #s{ vs = Vs0, es = Ls0, es2 = As0} = S) ->
    Vs1 = provisual_fgraph:del(Id, Vs0),
    Ls1 = provisual_fgraph:foldl(fun
	    ({{DId,_},_}, O) when DId =:= Id -> O;
	    ({{_,DId},_}, O) when DId =:= Id -> O;
	    ({K,V}, O) -> provisual_fgraph:add(K,V,O)
	end, provisual_fgraph:new(), Ls0),

    As1 = provisual_fgraph:foldl(fun
	    ({{DId,_},_}, O) when DId =:= Id -> O;
	    ({{_,DId},_}, O) when DId =:= Id -> O;
	    ({K,V}, O) -> provisual_fgraph:add(K,V,O)
	end, provisual_fgraph:new(), As0),
    S#s{ vs = Vs1, es = Ls1, es2 = As1 };

graph_event({add_edge, E, link}, #s{ es = Ls0 } = S) ->
    K   = 30.0,   % attractive force 
    L   =  5.0,   % spring length
    S#s{ es = provisual_fgraph:add(E, #fg_e{ k = K, l = L}, Ls0) };

graph_event({add_edge, E, ancestry}, #s{ es2 = Ls0 } = S) ->
    K   = 30.0,   % attractive force 
    L   =  5.0,   % spring length
    S#s{ es2 = provisual_fgraph:add(E, #fg_e{ k = K, l = L}, Ls0) };
	



graph_event(Other, S) ->
    io:format("Unhandled graph event ~p ~n", [Other]),
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

%% gl functions
draw_lines([{X1,Y1,Z1},{X2,Y2,Z2}|Pts]) ->
     gl:vertex3f(X1,Y1,Z1),
     gl:vertex3f(X2,Y2,Z2),
     draw_lines(Pts);
draw_lines([]) -> ok.

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


load_matrices(Cam) -> load_matrices(Cam,false).
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
	       elevation=El,pan_x=PanX,pan_y=PanY}, Lights) ->
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
	 distance=1350,
	 pan_x=0.0,pan_y=-2.0,
	 fov=45.0,
	 hither=0.1,
	 yon=10000.0,
	 ww=W,
	 wh=H}.
	 
%%%%%%%%%

%% graph nodes

add_node(G, Id, _, Name) ->
    G ! {graph, {add_node, Id, Name}},
    ok.

del_node(G, Id) ->
    G ! {graph, {del_node, Id}},
    ok.

add_link(G, E, Type) ->
    G ! {graph, {add_edge, E, Type}},
    ok.

del_link(G, E, Type) ->
    G ! {graph, {del_edge, E, Type}},
    ok.

clear(G) ->
    G ! {graph, clear},
    ok.

add_event(G, E, Type) ->
    ok.


%%


p1() -> float(random:uniform(160) - 80).
p3() -> {p1(), p1(), p1()}.
