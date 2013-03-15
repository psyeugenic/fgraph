%% Copyright (C) 2012 Björn-Egil Dahlberg
%%
%% File:    provisual.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2012-06-15

-module(provisual).
-export([
	start/0
    ]).

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


-record(camera, {
	origin,
	distance,     % From origo.
	azimuth,
	elevation,
	pan_x,	       % Panning in X direction.
	pan_y,	       % Panning in Y direction.
	fov,	       % Field of view.
	hither,       % Near clipping plane.
	yon,	       % Far clipping plane.
	xs = 0,           % Prev position
	ys = 0,
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
	camera, 
	model,
	sphere,
	edges,
	tracer
    }).

-record(rs, {mat_shader = false, shadows = false}).

-record(time, {
	fps = 0,      % frames per second
	fc = 0,       % frame counter
	diff = 0,     % Time last frame in ms
	start =erlang:now(),
	fcst  =erlang:now() % frame counter start time
    }). 


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
	    rstate = #rs{}, time=#time{}, camera = camera_init(?W,?H), 
	    model = provisual_model:new(),
	    sphere = load_sphere(5),
	    edges = fun() -> ok end
	}).

loop(quit) -> ok;
loop(#s{ frame=F, model = M,
	sphere = Sphere,
	edges = Edges,
	canvas = Canvas,
	camera = Cam, time=T} = S) ->
    %% Setup camera and light
    load_matrices(Cam),

    % draw edges (links|ancestry)
    gl:lineWidth(1.0),
    gl:color3f(1.0,1.0,1.0),
    Edges(),

    % draw edges (messages)
    gl:color3f(0.0,0.3,1.0),

    lists:foreach(fun({{K1,K2},I}) ->
		gl:lineWidth(8.0*(I/100)),
		#fg_v{p={X1,Y1,Z1}} = provisual_fgraph:get(K1, provisual_model:vs(M)),
		#fg_v{p={X2,Y2,Z2}} = provisual_fgraph:get(K2, provisual_model:vs(M)),
		gl:'begin'(?GL_LINES),
		draw_lines([
			{X1, Z1, Y1},{X2, Z2, Y2}
		    ]),
		gl:'end'()
	end, provisual_model:messages(M)),

    gl:color3f(1.0,0.0,0.4),
    provisual_fgraph:foreach(fun
	    ({_Key, #fg_v{ p ={X, Y, Z}}}) ->
		gl:pushMatrix(),
		gl:translatef(X,Z,Y),
		Sphere(),
		gl:popMatrix()
	end, provisual_model:vs(M)),

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
        Msg -> 
	    try
		handle_msg(Msg, S)
	    catch
		C:E ->
		    io:format("WTF ~p:~p~n~n~p~n", [C,E,erlang:get_stacktrace()]),
		    erlang:exit(badarg)
	    end
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

handle_msg( #wx{event=#wxMouse{ type = right_down}}, #s{ model = M} = S) ->
    S#s{ model = provisual_model:toggle_edge(M) };

handle_msg( #wx{event=Mouse=#wxMouse{}}, #s{ camera = Cam0}= S) ->
    {_, Cam} = cam_event(Mouse,Cam0),
    S#s{ camera = Cam};

handle_msg(#wx{event=#wxSize{size={W,H}}}, #s{ camera = Cam0}= S) ->
    gl:viewport(0,0,W,H),
    S#s{camera = Cam0#camera{ww=W,wh=H}};

handle_msg({Pid, force_step}, #s{ model = M0 } = S) ->
    M1 = provisual_model:step(M0),
    Lines = model_to_line_binary(M1),
    Pid ! {self(), ok},
    S#s{ model = M1, edges = Lines };

handle_msg({graph, Cmd},#s{ model = M} = S) ->
    % try to eat as many graph msgs as we can
    handle_events(S#s{ model = provisual_model:event(Cmd, M) });

handle_msg(#wx{ id=Id, event = #wxCommand{type = command_menu_selected}}, S) ->
    io:format("wx Id = ~p~n", [Id]),
    S;
handle_msg(Other, S) ->
    io:format("Got ~p ~n",[Other]),
    S.

cam_event(#wxMouse{type=motion, leftDown=false},Cam) ->
    {[], Cam};
cam_event(#wxMouse{type=mousewheel, wheelRotation=Rot},Cam=#camera{distance=Dist}) ->
    case Rot > 0 of
	true ->  {[],Cam#camera{distance=Dist+10}};
	false -> {[],Cam#camera{distance=Dist-10}}
    end;
cam_event(#wxMouse{type=motion, leftDown=true,shiftDown=true,x=X,y=Y},
	  Cam=#camera{pan_x=PanX0,pan_y=PanY0,distance=D,xs=Xs,ys=Ys}) ->
    %% PAN
    S = D*(1/20)/(101-float(?PAN_SPEED)),
    Dx = (X-Xs)*S,
    Dy = (Y-Ys)*S,
    PanX = PanX0 + Dx,
    PanY = PanY0 - Dy,
    {[],Cam#camera{pan_x=PanX,pan_y=PanY,xs=X,ys=Y}};
cam_event(#wxMouse{type=motion, leftDown=true,controlDown=true,x=X, y=Y},
	  Cam=#camera{distance=Dist, ys=Ys}) ->
    %% ZOOM (Dy)
    {[],Cam#camera{distance=Dist+(Y-Ys)/10,xs=X,ys=Y}};
cam_event(#wxMouse{type=motion, leftDown=true,x=X,y=Y},
	  Cam=#camera{azimuth=Az0,elevation=El0,xs=Xs,ys=Ys}) ->
    %% Rotate
    Az = Az0 + (X-Xs),
    El = El0 + (Y-Ys),
    {[], Cam#camera{azimuth=Az,elevation=El, xs=X,ys=Y}};
cam_event(#wxMouse{type=left_down, x=X,y=Y},Cam) ->
    {[],Cam#camera{xs=X,ys=Y}};
cam_event(Ev,Cam) ->
    {Ev,Cam}.

%% gl functions
draw_lines([{X1,Y1,Z1},{X2,Y2,Z2}|Pts]) ->
     gl:vertex3f(X1,Y1,Z1),
     gl:vertex3f(X2,Y2,Z2),
     draw_lines(Pts);
draw_lines([]) -> ok.


-define(F32, 32/float-native).

model_to_line_binary(M) -> 
    {Size, Data} = model_to_line_binary(provisual_model:vs(M), provisual_model:es(M)),
    [Buff] = gl:genBuffers(1),
    gl:bindBuffer(?GL_ARRAY_BUFFER,Buff),
    gl:bufferData(?GL_ARRAY_BUFFER, size(Data), Data, ?GL_STATIC_DRAW),
     fun() ->
             %% Draw buffer
             gl:bindBuffer(?GL_ARRAY_BUFFER,Buff),
             gl:vertexPointer(3, ?GL_FLOAT, 0, 0),
             gl:enableClientState(?GL_VERTEX_ARRAY),
             gl:drawArrays(?GL_LINES, 0, Size*3),
             gl:bindBuffer(?GL_ARRAY_BUFFER, 0),
             gl:disableClientState(?GL_VERTEX_ARRAY)
     end.

model_to_line_binary(Vs, Es) ->
    provisual_fgraph:foldl(fun
	    ({{K1,K2},_},{Size, Bin}) ->
		    #fg_v{p={X1,Y1,Z1}} = provisual_fgraph:get(K1, Vs),
		    #fg_v{p={X2,Y2,Z2}} = provisual_fgraph:get(K2, Vs),
		    {Size + 2, <<Bin/binary, X1:?F32, Z1:?F32, Y1:?F32 , X2:?F32, Z2:?F32, Y2:?F32>>}
	    end, {0, <<>>}, Es).

load_sphere(Scale) ->
    {Size, DataChunk, [Ns]} =
	provisual_sphere:tris([{subd,4}, {ccw,false}, {binary,true},  {scale,Scale}, {normals,true}]),
    StartNormals = size(DataChunk),
    Data = <<DataChunk/binary, Ns/binary>>,

    [Buff] = gl:genBuffers(1),
    gl:bindBuffer(?GL_ARRAY_BUFFER,Buff),
    gl:bufferData(?GL_ARRAY_BUFFER, size(Data), Data, ?GL_STATIC_DRAW),

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

projection(#camera{
	distance=D,
	fov=Fov,
	hither=Hither,
	yon=Yon, ww=W,wh=H,ortho=Ortho}) -> 
    Aspect = W/H,
    case Ortho of
	false ->
	    glu:perspective(Fov, Aspect, Hither, Yon);
	true ->
	    Sz = D*math:tan(Fov*math:pi()/180/2),
	    gl:ortho(-Sz*Aspect, Sz*Aspect, -Sz, Sz, Hither, Yon)
    end.

modelview(#camera{origin=Origin,distance=Dist,azimuth=Az,
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

camera_init(W,H) -> #camera{
	origin = {0.0, 0.0, 0.0},
	azimuth = -45.0,
	elevation = 25.0,
	distance = 1350,
	pan_x =  0.0,
	pan_y = -2.0,
	fov = 45.0,
	hither = 0.1,
	yon = 10000.0,
	ww = W,
	wh = H
    }.
	 
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
    G ! {graph, {add_event, E, Type}},
    ok.
