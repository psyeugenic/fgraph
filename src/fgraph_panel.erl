-module(fgraph_panel).
-export([
	new/1,
	add_node/2,
	del_node/2,
	add_edge/2,
	del_edge/2,
	set_dbl_click/2
	]).

-record(state,{
	frame,
	window,
	width,
	height,
	q_slider,
	mouse_act
	}).

-record(graph, {
	pen,
	brush,
	font,
	select = none,
	offset = {0,0},
	offset_state = false,
	ke = 0,
	vs = [],
	es = []
	}).

-include_lib("wx.hrl").
-include("fgraph.hrl").

-define(BRD,10).
-define(ARC_R, 10).

-define(move, 89).
-define(select, 90).
-define(q_slider, 91).
-define(l_slider, 92).
-define(k_slider, 93).
-define(lock, 94).

-define(color_bg, {45,50,95}).
-define(color_fg, {235,245,230}).
-define(color_active, {10,220,20}).
-define(color_active_bg, {20,230,30}).
-define(color_inactive, {220,10,20}).
-define(color_inactive_bg, {230,20,30}).

add_node(Pid, Node) -> Pid ! {add_node, Node}.
del_node(Pid, Node) -> Pid ! {del_node, Node}.

add_edge(Pid, {E1,E2}) -> Pid ! {add_edge, {E1,E2}}.
del_edge(Pid, {E1,E2}) -> Pid ! {del_edge, {E1,E2}}.

set_dbl_click(Pid, Fun) -> Pid ! {set_dbl_click, Fun}.

new(Parent) ->
    Env = wx:get_env(),
    Me  = self(),
    Pid = spawn_link(fun() -> init([Parent, Me, Env]) end),
    receive {Pid, {fgraph_panel, Panel}} -> {Pid,Panel} end.

init([Parent, Pid, Env]) ->
    wx:set_env(Env),
    
    WindowSizer = wxBoxSizer:new(?wxHORIZONTAL),
    ButtonSizer = wxBoxSizer:new(?wxVERTICAL),

    Panel = wxPanel:new(Parent),

    BMove = wxButton:new(Panel, ?move, [{label,"Move"}]),
    BSel  = wxButton:new(Panel, ?select, [{label,"Select"}]),
    BLock = wxButton:new(Panel, ?lock, [{label,"Lock"}]),
    SQ    = wxSlider:new(Panel, ?q_slider, 20, 1, 500, [{style, ?wxVERTICAL}]),
    SL    = wxSlider:new(Panel, ?l_slider, 20, 1, 500, [{style, ?wxVERTICAL}]),
    SK    = wxSlider:new(Panel, ?k_slider, 20, 1, 500, [{style, ?wxVERTICAL}]),
    Win   = wxWindow:new(Panel, ?wxID_ANY),
    
    SF = wxSizerFlags:new(),
    wxSizerFlags:proportion(SF,6),
    wxSizer:add(ButtonSizer, BMove, wxSizerFlags:center(wxSizerFlags:proportion(SF,0))),
    wxSizer:add(ButtonSizer, BSel,  wxSizerFlags:center(wxSizerFlags:proportion(SF,0))),
    wxSizer:add(ButtonSizer, BLock, wxSizerFlags:center(wxSizerFlags:proportion(SF,0))),
    wxSizer:add(ButtonSizer, SQ,    wxSizerFlags:proportion(wxSizerFlags:expand(SF),1)), 
    wxSizer:add(ButtonSizer, SL,    wxSizerFlags:proportion(wxSizerFlags:expand(SF),1)), 
    wxSizer:add(ButtonSizer, SK,    wxSizerFlags:proportion(wxSizerFlags:expand(SF),1)), 

    wxSizer:add(WindowSizer, ButtonSizer, wxSizerFlags:left(SF)),
    wxSizer:add(WindowSizer, Win,         wxSizerFlags:proportion(wxSizerFlags:expand(SF),10)),
    
    wxWindow:setSizer(Panel,WindowSizer),

    wxButton:connect(BMove, command_button_clicked),
    wxButton:connect(BSel,  command_button_clicked),
    wxButton:connect(BLock, command_button_clicked),
 
    wxWindow:connect(SQ, command_slider_updated),
    wxWindow:connect(SL, command_slider_updated),
    wxWindow:connect(SK, command_slider_updated),
   
    wxWindow:connect(Win, enter_window),        
    wxWindow:connect(Win, move),
    wxWindow:connect(Win, motion),
    wxWindow:connect(Win, key_up),
    wxWindow:connect(Win, mousewheel),
    wxWindow:connect(Win, left_down),
    wxWindow:connect(Win, left_up),
    wxWindow:connect(Win, right_down),
    wxWindow:connect(Win, size),
    wxWindow:connect(Win, paint,  [{skip, true}]),
    
    Pen   = wxPen:new({0,0,0}, [{width, 3}]),
    Font  = wxFont:new(10, ?wxSWISS, ?wxNORMAL, ?wxNORMAL,[]),
    Brush = wxBrush:new({0,0,0}),

    Pid ! {self(), {fgraph_panel, Panel}},

    wxWindow:setFocus(Win), %% Get keyboard focus
  
    Vs = fgraph:new(),
    Es = fgraph:new(),

    Me = self(),
    spawn_link(fun() -> ticker_init(Me) end),
    
    loop( #state{ q_slider = SQ, mouse_act = ?select, frame = Parent, window = Win},  #graph{ vs = Vs, es = Es, pen = Pen, font = Font, brush = Brush}).

graph_add_node_unsure(Key, G = #graph{ vs = Vs }) ->
    case fgraph:is_defined(Key, Vs) of
	true -> G;
	_    -> graph_add_node(Key, G)
    end.
graph_add_node(Key, G = #graph{ vs = Vs}) ->
    Q  = 20.0,   % repulsive force
    M  = 0.5,    % mass
    P  = {float(450 + random:uniform(100)),float(450 + random:uniform(100))},
    G#graph{ vs = fgraph:add(Key, #fg_v{ p = P, m = M, q = Q}, Vs)}.

graph_del_node(Key, G = #graph{ vs = Vs0, es = Es0}) ->
    Vs = fgraph:del(Key, Vs0),
    Es = delete_edges(Es0, [Key]),
    G#graph{ vs = Vs, es = Es }.

graph_add_edge(Key0, Key1, G = #graph{ es = Es}) ->
    K  = 60.0,   % attractive force 
    L  =  5.0,   % spring length
    G#graph{ es = fgraph:add({Key0, Key1}, #fg_e{ k = K, l = L}, Es) }.

graph_del_edge(Key0, Key1, G = #graph{ es = Es}) ->
    G#graph{ es = fgraph:del({Key0, Key1}, Es) }.



ticker_init(Pid) ->
    ticker_loop(Pid, 50).
ticker_loop(Pid, Time) ->
    receive after Time ->
	Pid ! {self(), redraw},
	T0 = now(),
	receive {Pid, ok} -> ok end,
	T1 = now(),
	D = timer:now_diff(T1, T0)/1000,
	case trunc(40 - D) of
	    Ms when Ms < 0 ->
		%io:format("ticker: wait is   0 ms [fg ~7s ms] [fps ~7s]~n", [s(D), s(1000/D)]),
		ticker_loop(Pid, 0);
	    Ms ->
		%io:format("ticker: wait is ~3s ms [fg ~7s ms] [fps ~7s]~n", [s(Ms), s(D), s(1000/40)]),
		ticker_loop(Pid, Ms)
	end
    end.

delete_edges(Es, []) -> Es;
delete_edges(Es, [Key|Keys]) ->
    Edges = fgraph:foldl(fun
	({{K1, K2}, _}, Out) when K1 =:= Key -> [{K1,K2}|Out];
	({{K1, K2}, _}, Out) when K2 =:= Key -> [{K1,K2}|Out];
	(_, Out) -> Out
    end, [], Es),
    Es1 = lists:foldl(fun
	(K, Esi) -> fgraph:del(K, Esi)
    end, Es, Edges),
    delete_edges(Es1, Keys).
    

set_charge(Q, Vs) ->
    fgraph:map(fun
	({Key, Value}) -> {Key, Value#fg_v{ q = Q}}
    end, Vs).
    

loop(State, Graph) ->
    receive
	#wx{id = ?lock, event = #wxCommand{type=command_button_clicked}} ->
	    Vs = fgraph:map(fun
		({Key, V = #fg_v{ selected = true, type = dynamic}}) -> {Key, V#fg_v{ type = static  }};
		({Key, V = #fg_v{ selected = true, type = static }}) -> {Key, V#fg_v{ type = dynamic }};
		(KV) -> KV
	    end, Graph#graph.vs),
	    loop(State, Graph#graph{ vs = Vs });
	#wx{id = ?select, event = #wxCommand{type=command_button_clicked}} ->
	    loop(State#state{ mouse_act = ?select }, Graph);
	
	#wx{id = ?move, event = #wxCommand{type=command_button_clicked}} ->
	    loop(State#state{ mouse_act = ?move }, Graph);

	#wx{id = ?q_slider, event = #wxCommand{type=command_slider_updated, commandInt = Q}} ->
	    loop(State, Graph#graph{ vs = set_charge(Q, Graph#graph.vs)});
	#wx{id = ?l_slider, event = #wxCommand{type=command_slider_updated, commandInt = L}} ->
	    F  = fun
		({Ps, E}) -> {Ps, E#fg_e{ l = L}}
	     end,
	    Es = fgraph:map(F, Graph#graph.es),
	    loop(State, Graph#graph{ es = Es});
	#wx{id = ?k_slider, event = #wxCommand{type=command_slider_updated, commandInt = K}} ->
	    F  = fun
		({Ps, E}) -> {Ps, E#fg_e{ k = K}}
	     end,
	    Es = fgraph:map(F, Graph#graph.es),
	    loop(State, Graph#graph{ es = Es});
	
	#wx{event=#wxKey{type=key_up, keyCode = 127}} -> % delete
	    {Vs1, Keys} = fgraph:foldl(fun
		({Key, #fg_v{ selected = true}}, {Vs, Ks}) -> {fgraph:del(Key,Vs), [Key|Ks]};
		(_, {Vs, Ks}) -> {Vs, Ks}
	    end, {Graph#graph.vs,[]}, Graph#graph.vs),
	    Es = delete_edges(Graph#graph.es, Keys),
	    loop(State, Graph#graph{ vs = Vs1, es = Es});
	    
	#wx{event=#wxKey{type=key_down}} ->
	    loop(State, Graph);

	%%% mouse
	#wx{event=#wxMouse{type=left_down,x=X,y=Y}} -> 
	    case State#state.mouse_act of
		?select -> loop(State, mouse_left_down_select(Graph, {X,Y}));
		?move   -> loop(State, mouse_left_down_move(Graph, {X,Y}))
	    end;
	#wx{event=#wxMouse{type=motion, x=X, y=Y}} ->
	    case State#state.mouse_act of
		?select -> loop(State, mouse_motion_select(Graph, {X,Y}));
		?move   -> loop(State, mouse_motion_move(Graph, {X,Y}))
	    end;
	#wx{event=#wxMouse{type=left_up, x=X, y=Y}} -> 
	    case State#state.mouse_act of
		?select -> loop(State, mouse_left_up_select(Graph, {X,Y}));
		?move   -> loop(State, mouse_left_up_move(Graph, {X,Y}))
	    end;

	#wx{event=#wxMouse{type=right_down,x=X,y=Y}} -> 
	    loop(State, Graph);
	%% mouse wheel
	#wx{event=#wxMouse{type=mousewheel, wheelRotation=Rotation}} ->
	    Q = wxSlider:getValue(State#state.q_slider),
	    case {Q, Rotation} of
		{Q, Rotation} when Rotation < 0, Q > 5 ->
		    wxSlider:setValue(State#state.q_slider, Q - 4),
	    	    loop(State, Graph#graph{ vs = set_charge(Q - 5, Graph#graph.vs) });
		{Q, Rotation} when Rotation > 0 ->
		    wxSlider:setValue(State#state.q_slider, Q + 4),
	    	    loop(State, Graph#graph{ vs = set_charge(Q + 4, Graph#graph.vs) });
		_ -> 
		    loop(State, Graph)
	    end;
		    
	#wx{event=#wxClose{}} ->
	    catch wxWindow:'Destroy'(State#state.frame);
	#wx{id=?wxID_EXIT, event=#wxCommand{type=command_menu_selected}} ->
	    wxWindow:close(State#state.frame,[]);
 	#wx{obj=Win,event=#wxPaint{}} ->
	    redraw(State, Graph),
	    loop(State, Graph);
	#wx{obj=Win, event=#wxSize{}} ->
	    loop(State, Graph);
	#wx{obj=Win,event=#wxMouse{type=enter_window}} ->
	    wxWindow:setFocus(Win), 
	    loop(State, Graph);


	%% Graph manipulation
	{add_node, Key}     -> loop(State, graph_add_node_unsure(Key, Graph));
	{del_node, Key}     -> loop(State, graph_del_node(Key, Graph));
	{add_edge, {K0,K1}} -> loop(State, graph_add_edge(K0, K1, Graph));
	{del_edge, {K0,K1}} -> loop(State, graph_del_edge(K0, K1, Graph));
% del edge	
%	    case fgraph:get(Pid0, Graph#graph.vs) of
%		undefined -> loop(State, Graph);
%		V ->
%		    loop(State, Graph#graph{ vs = fgraph:set(Pid0, V#fg_v{ state = dead }, Graph#graph.vs) }) 
%	    end;

   	{Req, redraw} ->
		Vs = fgraph:step(Graph#graph.vs, Graph#graph.es),
		Req ! {self(), ok},
	        redraw(State, Graph),
	        loop(State, Graph#graph{ vs = Vs} );
	Other ->
	    loop(State, Graph)
   end.

mouse_left_down_select(G, {X0,Y0}) ->
    G#graph{ select   = {{X0,Y0}, {X0,Y0}} }.

mouse_left_down_move(G = #graph{ vs = Vs}, {X,Y}) ->
    {Xo, Yo} = G#graph.offset,
    Xr = X - Xo,
    Yr = Y - Yo,

    % point on node?
    case fgraph:foldl(fun
	({Key, #fg_v{ p = {Px, Py}}}, _) when abs(Px - Xr) < 10, abs(Py - Yr) < 10 -> {true, Key};
	(_, Out) -> Out
	end, false, Vs) of
	false       -> G#graph{ offset_state = {X,Y}};
	{true, Key} -> V = #fg_v{ type = Type} = fgraph:get(Key,Vs),  G#graph{ vs = fgraph:set(Key, V#fg_v{ type = moving}, Vs), select = {node, Key, Type} }
    end.



mouse_left_up_select(G, {X,Y}) ->
    case G#graph.select of
        {{X0,Y0}, {X1, Y1}} ->
	    {Xo, Yo} = G#graph.offset,
	    Xmin = lists:min([X0,X1]) - Xo,
	    Xmax = lists:max([X1,X0]) - Xo,
	    Ymin = lists:min([Y0,Y1]) - Yo,
	    Ymax = lists:max([Y1,Y0]) - Yo,
	    Vs = fgraph:map(fun
		({Key, Value = #fg_v{ p = {Px, Py}}}) when Px > Xmin, Px < Xmax, Py > Ymin, Py < Ymax ->
		    {Key, Value#fg_v{ selected = true }};
		({Key, Value}) -> {Key, Value#fg_v{ selected = false }}
	    end, G#graph.vs),
	    G#graph{ select = none, vs = Vs};
	_ ->
	    G#graph{ select = none}
    end.
	    
	
mouse_left_up_move(G = #graph{ select = Select, vs = Vs} = G, {X,Y}) ->
    case Select of
	{node, Key, Type} ->
	    V = fgraph:get(Key, Vs),
	    G#graph{ select = none, vs = fgraph:set(Key, V#fg_v{ type = Type}, Vs), offset_state = false };
	_ ->
    	    G#graph{ select = none, offset_state = false }
    end.
	
mouse_motion_select(G, {X,Y}) ->
    case G#graph.select of
	{P0, _P1} -> G#graph{ select = {P0, {X,Y}}};
	_        -> G
    end.
mouse_motion_move(G = #graph{ select = {node, Key, _}, vs = Vs}, {X,Y}) ->
    {Xo, Yo} = G#graph.offset,
    V = fgraph:get(Key, Vs),
    G#graph{ vs = fgraph:set(Key, V#fg_v{ p = {float(X - Xo), float(Y - Yo)}}, Vs) };
mouse_motion_move(G, {X,Y}) ->
    case G#graph.offset_state of
	{X1,Y1} -> 
	    {X0, Y0} = G#graph.offset,
	    G#graph{  offset_state = {X,Y}, offset = {X0 - (X1 - X), Y0 - (Y1 - Y)} };
	    _ -> 
		G
    end.
	
redraw(S = #state{window=Win}, G) ->
    DC0  = wxClientDC:new(Win),
    DC   = wxBufferedDC:new(DC0),
    Size = wxWindow:getSize(Win),
    redraw(DC, Size, G),
    wxBufferedDC:destroy(DC),
    wxClientDC:destroy(DC0),
    ok.

redraw(DC, Size, G) ->    
    wx:batch(fun() -> 
   
    	Pen   = G#graph.pen,
	Font  = G#graph.font,
	Brush = G#graph.brush,
	wxDC:setTextForeground(DC,?color_fg),
	wxBrush:setColour(Brush, ?color_bg),
	wxDC:setBrush(DC, Brush),
	wxDC:setBackground(DC, Brush),
	wxPen:setWidth(Pen, 1),
	wxDC:clear(DC),

	% draw vertices and edges 
    	wxPen:setColour(Pen, ?color_fg),
    	wxDC:setPen(DC,Pen),

	%draw_es(DC, G#graph.es_pts, G#graph.offset),
	draw_es(DC, G#graph.vs, G#graph.es, G#graph.offset),
        draw_vs(DC, G#graph.vs, G#graph.offset, Pen, Brush),


	% draw selection box
    	wxPen:setColour(Pen, ?color_fg),
    	wxDC:setPen(DC,Pen),
	draw_select_box(DC, G#graph.select),

	% draw information text
	wxFont:setWeight(Font,?wxNORMAL),
	draw_text(DC, fgraph:size(G#graph.vs), fgraph:size(G#graph.es), G#graph.ke),
	ok
    end).

draw_select_box(DC, {{X0,Y0}, {X1,Y1}}) ->
    draw_line(DC, {X0,Y0}, {X1,Y0}, {0,0}),
    draw_line(DC, {X1,Y1}, {X1,Y0}, {0,0}),
    draw_line(DC, {X1,Y1}, {X0,Y1}, {0,0}),
    draw_line(DC, {X0,Y0}, {X0,Y1}, {0,0}),
    ok;
draw_select_box(DC, _) -> ok.

draw_es(DC, Vs, Es, Po) ->
    fgraph:foreach(fun
	({{K1, K2}, _}) ->
	    #fg_v{ p = P1} = fgraph:get(K1, Vs),
	    #fg_v{ p = P2} = fgraph:get(K2, Vs),
	    draw_line(DC, P1, P2, Po)
	end, Es).

draw_line(DC, {X0,Y0}, {X1, Y1}, {X, Y}) ->
    wxDC:drawLine(DC, {trunc(X0 + X), trunc(Y0 + Y)}, {trunc(X1 + X), trunc(Y1 + Y)}).
   
draw_vs(DC, Vs, {Xo, Yo}, Pen, Brush) ->
    fgraph:foreach(fun
	({Key, #fg_v{ p ={X, Y}, state = State, selected = Sel}}) ->
	    String = s(Key),
	    case Sel of
		true ->
	    	    wxPen:setColour(Pen, ?color_fg),
		    wxBrush:setColour(Brush, ?color_bg),
	    	    wxDC:setPen(DC,Pen),
	    	    wxDC:setBrush(DC, Brush),
	    	    wxDC:drawRoundedRectangle(DC, {trunc(X-12 + Xo), trunc(Y-12 + Yo),24,24}, float(?ARC_R)),
		    ok;
		_ -> ok
	    end,
	    case State of
		running -> 
	    	    wxPen:setColour(Pen, ?color_active),
	    	    wxBrush:setColour(Brush, ?color_active_bg);
		_ -> 
	    	    wxPen:setColour(Pen, ?color_inactive),
	    	    wxBrush:setColour(Brush, ?color_inactive_bg)
	    end,
	    wxDC:setPen(DC,Pen),
	    wxDC:setBrush(DC, Brush),
	    wxDC:drawRoundedRectangle(DC, {trunc(X-8 + Xo),trunc(Y-8 + Yo),17,17}, float(?ARC_R)),
    	    wxDC:drawText(DC, String, {trunc(X + Xo), trunc(Y + Yo)}),
	    ok;
	(_) -> ok
    end, Vs).

draw_text(DC, Nvs, Nes, KE) ->
    VsString = "#vs: " ++ integer_to_list(Nvs),
    EsString = "#es: " ++ integer_to_list(Nes),
    KEString = " ke: " ++ s(KE),
    wxDC:drawText(DC, VsString, {10,10}),
    wxDC:drawText(DC, EsString, {10,25}),
    wxDC:drawText(DC, KEString, {10,40}),
    ok.

s(Format, Terms) -> lists:flatten(io_lib:format(Format, Terms)).
s(Term) when is_float(Term) -> s("~.2f", [Term]);
s(Term) when is_integer(Term) -> integer_to_list(Term);
s(Term) when is_atom(Term) -> atom_to_list(Term);
s(Term) -> s("~p", [Term]).
