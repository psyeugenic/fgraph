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
    _Ticker = provisual_ticker:start(Me),
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
		
		provisual_tracer:start(Node, Pid, Rs)
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
