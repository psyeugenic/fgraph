%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%

-module(provisual_fgraph_win).

-export([
         new/2,
         add_node/2,
         add_node/3,
         add_node/4,
	 add_event/2,
         del_node/2,
         change_node/3,
         add_link/2,
         del_link/2,
         add_link/3,
         del_link/3,
	 set_links/2,
	 
	 clear/1,

         set_click/2,
	 set_runnability/2,
         stop/2
        ]).

-include_lib("wx/include/wx.hrl").
-include_lib("provisual_fgraph.hrl").

-record(state,
        {
          parent_pid,
          frame,
          window,
          width,
          height,
          q_slider,
          k_slider,
          mouse_act,
          is_frozen,
          ticker
         }).

-record(graph,
        {
          pen,
          brush,
          font,
          select = none,
          offset = {0,0},
          offset_state = false,
          ke = 0,
          vs = [],
          es = default,
	  es_wheel = gb_trees:enter(default, [], gb_trees:empty()),
	  runnability = [],
	  cr = 0,
	  click_fun = fun(Key) -> Key end,
	  events
         }).

-define(BRD,10).
-define(ARC_R, 10).

-define(reset, 80).
-define(lock, 81).
-define(unlock, 82).
-define(move, 83).
-define(select, 84).
-define(delete, 85).
-define(freeze, 86).

-define(q_slider, 90).
-define(k_slider, 92).

-define(default_q, 20).
-define(default_l, 20).
-define(default_k, 20).

-define(color_bg, {45,50,95}).
-define(color_fg, {235,245,230}).
-define(color_default, {10,220,20}).
-define(color_default_bg, {20,230,30}).
-define(color_alternate, {220,10,20}).
-define(color_alternate_bg, {230,20,30}).

add_node(Pid, Key) -> add_node(Pid, Key, default).
add_node(Pid, Key, Color) -> add_node(Pid, Key, Color, Key).
add_node(Pid, Key, Color, Name) -> Pid ! {add_node, Key, Color, Name}.
del_node(Pid, Key) -> Pid ! {del_node, Key}.
change_node(Pid, Key, Color) ->  Pid ! {change_node, Key, Color}.

add_event(Pid, Keys) -> Pid ! {add_event, Keys}.

add_link(Pid, Keys) -> add_link(Pid, Keys, default).
del_link(Pid, Keys) -> add_link(Pid, Keys, default).

add_link(Pid, {FromKey, ToKey}, Wheel) -> 
    Pid ! {add_link, {FromKey, ToKey}, Wheel}.
del_link(Pid, {FromKey, ToKey}, Wheel) -> 
    Pid ! {del_link, {FromKey, ToKey}, Wheel}.

clear(Pid) -> Pid ! clear_nodes_and_links.


set_links(Pid, Wheel) -> Pid ! {set_links, Wheel}.

set_runnability(Pid, N) -> Pid ! {set_runnability, N}.

stop(Pid, Reason) -> 
    Ref = erlang:monitor(process, Pid),
    Pid ! {stop, Reason},
    receive
        {'DOWN', Ref, _, _, _} ->
            ok
    end.

set_click(Pid, Fun) when is_function(Fun) -> Pid ! {set_click, Fun}.

new(Parent, Options) ->
    Env = wx:get_env(),
    Me  = self(),
    Pid = spawn_link(fun() -> init([Parent, Me, Env, Options]) end),
    receive {Pid, {?MODULE, Panel}} -> {Pid,Panel} end.
    
init([ParentWin, Pid, Env, Options]) ->
    wx:set_env(Env),
    
    BReset  = wxButton:new(ParentWin, ?reset,  [{label,"Reset"}]),
    BFreeze = wxButton:new(ParentWin, ?freeze, [{label,"Freeze"}]),
    BLock   = wxButton:new(ParentWin, ?lock,   [{label,"Lock"}]),
    BUnlock = wxButton:new(ParentWin, ?unlock, [{label,"Unlock"}]),
    BDelete = wxButton:new(ParentWin, ?delete, [{label,"Delete"}]),

    SQ  = wxSlider:new(ParentWin, ?q_slider, ?default_q, 1, 500, [{style, ?wxVERTICAL}]),
    SK  = wxSlider:new(ParentWin, ?k_slider, ?default_k, 1, 500, [{style, ?wxVERTICAL}]),
    Win = wxWindow:new(ParentWin, ?wxID_ANY, Options),
    
    ButtonSizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(ButtonSizer, BReset),
    wxSizer:add(ButtonSizer, BFreeze),
    wxSizer:add(ButtonSizer, BLock),
    wxSizer:add(ButtonSizer, BUnlock),
    wxSizer:add(ButtonSizer, BDelete),

    SliderSizer = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(SliderSizer, SQ, [{flag, ?wxEXPAND}, {proportion, 1}]),
    %wxSizer:add(SliderSizer, SL, [{flag, ?wxEXPAND}, {proportion, 1}]),
    wxSizer:add(SliderSizer, SK, [{flag, ?wxEXPAND}, {proportion, 1}]),
    wxSizer:add(ButtonSizer, SliderSizer, [{flag, ?wxEXPAND}, {proportion, 1}]),

    WindowSizer = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(WindowSizer, ButtonSizer, [{flag, ?wxEXPAND}, {proportion, 0}]),
    wxSizer:add(WindowSizer, Win, [{flag, ?wxEXPAND}, {proportion, 1}]),
    
    wxButton:setToolTip(BReset, "Remove selection and unlock all nodes."),
    wxButton:setToolTip(BFreeze, "Start/stop redraw of screen."),
    wxButton:setToolTip(BLock, "Lock all selected nodes."),
    wxButton:setToolTip(BUnlock, "Unlock all selected nodes."),
    wxButton:setToolTip(BDelete, "Delete all selected nodes."),

    wxButton:setToolTip(SQ, "Control repulsive force. This can also be controlled with the mouse wheel on the canvas."),
    %wxButton:setToolTip(SL, "Control link length."),
    wxButton:setToolTip(SK, "Control attractive force. Use with care."),
    wxButton:setToolTip(Win, 
			"Drag mouse while left mouse button is pressed to perform various operations. "
			"Combine with control key to select. Combine with shift key to lock single node."),

    wxButton:connect(BReset,  command_button_clicked),
    wxButton:connect(BFreeze, command_button_clicked),
    wxButton:connect(BLock,   command_button_clicked),
    wxButton:connect(BUnlock, command_button_clicked),
    wxButton:connect(BDelete, command_button_clicked),
 
    wxWindow:connect(SQ, command_slider_updated),
    wxWindow:connect(SK, command_slider_updated),
   
    wxWindow:connect(Win, enter_window),        
    wxWindow:connect(Win, move),
    wxWindow:connect(Win, motion),
    wxWindow:connect(Win, mousewheel),
    wxWindow:connect(Win, key_up),
    wxWindow:connect(Win, left_down),
    wxWindow:connect(Win, left_up),
    wxWindow:connect(Win, right_down),
    wxWindow:connect(Win, paint,  [{skip, true}]),
    
    Pen   = wxPen:new({0,0,0}, [{width, 3}]),
    Font  = wxFont:new(12, ?wxSWISS, ?wxNORMAL, ?wxNORMAL,[]),
    Brush = wxBrush:new({0,0,0}),

    Pid ! {self(), {?MODULE, WindowSizer}},

    wxWindow:setFocus(Win), %% Get keyboard focus
  
    Vs = provisual_fgraph:new(),

    Me = self(),
    Ticker = spawn_link(fun() -> ticker_init(Me) end),
    
    loop( #state{ parent_pid = Pid,
                  q_slider = SQ,
		  k_slider = SK,
                  mouse_act = ?move,
		  frame = ParentWin,
		  window = Win,
                  is_frozen = false,
		  ticker = Ticker},
          #graph{ vs = Vs,
		  es = default,
		  es_wheel = gb_trees:enter(default, provisual_fgraph:new(), gb_trees:empty()),
		  pen = Pen,
		  font = Font,
		  events = gb_trees:empty(),
		  brush = Brush}).

graph_add_node_unsure(Key, State, Name, G = #graph{ vs = Vs }) ->
    case provisual_fgraph:is_defined(Key, Vs) of
        true  -> G;
        false -> graph_add_node(Key, State, Name, G)
    end.

graph_add_node(Key, Color, Name, G = #graph{ vs = Vs}) ->
    Q  = 20.0,   % repulsive force
    M  = 0.5,    % mass
    P  = {float(450 + random:uniform(100)),
	  float(450 + random:uniform(100))},
    G#graph{ vs = provisual_fgraph:add(Key, #fg_v{ p = P, m = M, q = Q, color = Color, name = Name}, Vs)}.

graph_change_node(Key, Color, G) ->
    case provisual_fgraph:get(Key, G#graph.vs) of
	undefined -> 
	    G;
	V ->
	    G#graph{ vs = provisual_fgraph:set(Key, V#fg_v{ color = Color }, G#graph.vs)}
    end.

graph_del_node(Key, G = #graph{ vs = Vs0, es_wheel = Tree0}) ->
    Vs = provisual_fgraph:del(Key, Vs0),
    NewTree = lists:foldl(fun
    	({Wheel, Es0}, Tree) ->
    	    Es1 = delete_edges(Es0, [Key]),
	    gb_trees:enter(Wheel, Es1, Tree)
	end, Tree0, gb_trees:to_list(Tree0)),

    G#graph{ vs = Vs, es_wheel = NewTree }.

graph_add_link(Key0, Key1, Es) ->
    K  = 60.0,   % attractive force 
    L  =  5.0,   % spring length
    provisual_fgraph:add({Key0, Key1}, #fg_e{ k = K, l = L}, Es).

graph_del_link(Key0, Key1, Es) ->
    provisual_fgraph:del({Key0, Key1}, Es).

ticker_init(Pid) ->
    ticker_loop(Pid, 50).
ticker_loop(Pid, Time) ->
    receive after Time ->
        Pid ! {self(), redraw},
        T0 = now(),
        receive {Pid, ok} -> ok end,
        T1 = now(),
        D = timer:now_diff(T1, T0)/1000,
        case round(40 - D) of
            Ms when Ms < 0 -> ticker_loop(Pid, 0);
            Ms -> ticker_loop(Pid, Ms)
        end
    end.

delete_edges(Es, []) -> 
    Es;
delete_edges(Es, [Key|Keys]) ->
    Edges = provisual_fgraph:foldl(fun
        ({{K1, K2}, _}, Out) when K1 =:= Key -> [{K1,K2}|Out];
        ({{K1, K2}, _}, Out) when K2 =:= Key -> [{K1,K2}|Out];
        (_, Out) -> Out
    end, [], Es),
    Es1 = lists:foldl(fun
        (K, Esi) -> provisual_fgraph:del(K, Esi)
    end, Es, Edges),
    delete_edges(Es1, Keys).
    

set_charge(Q, #graph{ vs = Vs} = G) -> % Repulsive force
    F = fun({Key, Value}) -> {Key, Value#fg_v{ q = Q}} end,
    G#graph{ vs = provisual_fgraph:map(F, Vs)}.


set_spring(K, #graph{ es_wheel = Wheel} = G) -> % Attractive force
    G#graph{ es_wheel = lists:foldl(fun
	({Key, Es}, Tree) ->
	    F  = fun({Ps, E}) -> {Ps, E#fg_e{ k = K}} end,
	    gb_trees:enter(Key, provisual_fgraph:map(F, Es), Tree)
	end, Wheel, gb_trees:to_list(Wheel)) }.

loop(S, G) ->
    receive
        %Msg ->
	%io:format("msg: ~p~n", [Msg]),
	%case Msg of
        #wx{id = ?reset, event = #wxCommand{type=command_button_clicked}} ->
	    %% Remove selection and unlock all nodes
            Q = ?default_q,
            K = ?default_k,
            wxSlider:setValue(S#state.q_slider, Q),
            wxSlider:setValue(S#state.k_slider, K),

            G2 = set_spring(K, G),
	    
            Vs2 = provisual_fgraph:map(fun
	    	({Key, V}) ->
		     {Key, V#fg_v{selected = false, type = dynamic, q = Q}}
	    end, G2#graph.vs),

            {Xs, Ys} = provisual_fgraph:foldl(fun({_Key, #fg_v{p = {X, Y}}}, {Xs, Ys}) ->
	    				    {[X| Xs], [Y | Ys]}
	    			    end,
	    			    {[], []},
	    			    Vs2),
	   %% io:format("Before: ~p\n", [G#graph.offset]),
	    Offset =
                case length(Xs) of
                    0 ->
                        {0, 0};
                    N ->
			MeanX = (lists:sum(Xs) / N), 
			MeanY = (lists:sum(Ys) / N),
			{SizeX, SizeY} = wxWindow:getSize(S#state.window),
			{0 - MeanX + (SizeX / 2), 0 - MeanY + (SizeY / 2)}
                end,
	    %% io:format("After: ~p\n", [Offset]),
	    loop(S, G2#graph{vs = Vs2, offset = Offset, offset_state = false});

        #wx{id = ?freeze, event = #wxCommand{type=command_button_clicked}} ->
	    %% Start/stop redraw of screen
            IsFrozen =
                case S#state.is_frozen of
                    true ->
                        S#state.ticker ! {self(), ok},
                        false;
                    false ->
                        true
                end,
            loop(S#state{is_frozen = IsFrozen}, G);
        #wx{id = ?lock, event = #wxCommand{type=command_button_clicked}} ->
	    %% Lock all selected nodes
            Vs = provisual_fgraph:map(fun
                            ({Key, V = #fg_v{selected = true}}) ->
				   {Key, V#fg_v{ type = static  }};
                            (KV) -> KV
                           end, G#graph.vs),
            loop(S, G#graph{ vs = Vs });
        #wx{id = ?unlock, event = #wxCommand{type=command_button_clicked}} ->
	    %% Unlock all selected nodes
            Vs = provisual_fgraph:map(fun
                            ({Key, V = #fg_v{selected = true}}) ->
				   {Key, V#fg_v{ type = dynamic }};
                            (KV) -> KV
                           end, G#graph.vs),
            loop(S, G#graph{ vs = Vs });
        #wx{id = ?delete, event = #wxCommand{type=command_button_clicked}} ->
	    %% Delete all selected nodes
            {Vs1, Keys} = provisual_fgraph:foldl(fun
                                       ({Key, #fg_v{ selected = true}}, {Vs, Ks}) ->
					      {provisual_fgraph:del(Key,Vs), [Key|Ks]};
                                       (_, {Vs, Ks}) -> {Vs, Ks}
                                      end, {G#graph.vs,[]}, G#graph.vs),
	    Tree0 = G#graph.es_wheel,
	    NewTree = lists:foldl(fun
    		({Wheel, Es0}, Tree) ->
    	    	    Es1 = delete_edges(Es0, Keys),
	    	    gb_trees:enter(Wheel, Es1, Tree)
	    end, Tree0, gb_trees:to_list(Tree0)),

            loop(S, G#graph{ vs = Vs1, es_wheel = NewTree});

        #wx{id = ?select, event = #wxCommand{type=command_button_clicked}} ->
            loop(S#state{ mouse_act = ?select }, G);

        #wx{id = ?move, event = #wxCommand{type=command_button_clicked}} ->
            loop(S#state{ mouse_act = ?move }, G);

        #wx{id = ?q_slider, event = #wxCommand{type=command_slider_updated, commandInt = Q}} ->
            loop(S, set_charge(Q, G));
        #wx{id = ?k_slider, event = #wxCommand{type=command_slider_updated, commandInt = K}} ->
            loop(S, set_spring(K, G));
        #wx{event=#wxKey{type=key_up, keyCode = 127}} -> % delete
            {Vs1, Keys} =
		provisual_fgraph:foldl(fun({Key, #fg_v{ selected = true}}, {Vs, Ks}) ->
				     {provisual_fgraph:del(Key,Vs), [Key|Ks]};
				(_, {Vs, Ks}) ->
				     {Vs, Ks}
			     end,
			     {G#graph.vs,[]}, G#graph.vs),

	    Tree0 = G#graph.es_wheel,
	    NewTree = lists:foldl(fun
    		({Wheel, Es0}, Tree) ->
    	    	    Es1 = delete_edges(Es0, Keys),
	    	    gb_trees:enter(Wheel, Es1, Tree)
	    end, Tree0, gb_trees:to_list(Tree0)),

            loop(S, G#graph{ vs = Vs1, es_wheel = NewTree});

        #wx{event=#wxKey{type=key_up}} ->
            loop(S, G);
        #wx{event=#wxKey{type=key_down}} ->
            loop(S, G);

        %% mouse
        #wx{event=#wxMouse{type=left_down, shiftDown=Shift, controlDown=Ctrl, x=X, y=Y}} ->
            if
                Shift ->
                    loop(S, mouse_left_down_move(G, {X,Y}));
                Ctrl ->
                    loop(S, mouse_left_down_select(G, {X,Y}));
                S#state.mouse_act =:= ?move ->
                    loop(S, mouse_left_down_move(G, {X,Y}));
                S#state.mouse_act =:= ?select ->
                    loop(S, mouse_left_down_select(G, {X,Y}))
            end;
        #wx{event=#wxMouse{type=motion, shiftDown=Shift, controlDown=Ctrl, x=X, y=Y}} ->
            if
                Shift ->
                    loop(S, mouse_motion_move(G, {X,Y}));
                Ctrl ->
                    loop(S, mouse_motion_select(G, {X,Y}));
                S#state.mouse_act =:= ?move ->
                    loop(S, mouse_motion_move(G, {X,Y}));
                S#state.mouse_act =:= ?select ->
                    loop(S, mouse_motion_select(G, {X,Y}))
            end;
        #wx{event=#wxMouse{type=left_up, shiftDown=Shift, controlDown=Ctrl, x=X, y=Y}} -> 
            if
                Shift ->
                    loop(S, mouse_left_up_move(G, {X,Y}, Shift));
                Ctrl ->
                    loop(S, mouse_left_up_select(G, {X,Y}));
                S#state.mouse_act =:= ?move ->
                    loop(S, mouse_left_up_move(G, {X,Y}, Shift));
                S#state.mouse_act =:= ?select ->
                    loop(S, mouse_left_up_select(G, {X,Y}))
            end;

        #wx{event=#wxMouse{type=right_down,x=_X,y=_Y}} -> 
            loop(S, G);
        %% mouse wheel
        #wx{event=#wxMouse{type=mousewheel, wheelRotation=Rotation}} ->
            Q = wxSlider:getValue(S#state.q_slider),
            if
		Rotation > 0, Q > 5 ->
                    wxSlider:setValue(S#state.q_slider, Q - 4),
                    loop(S, set_charge(Q - 4, G));
		Rotation < 0 ->
                    wxSlider:setValue(S#state.q_slider, Q + 4),
                    loop(S, set_charge(Q + 4, G));
                true -> 
                    loop(S, G)
            end;

        #wx{obj=_Win,event=#wxPaint{}} ->
            %%redraw(S, G),
            loop(S, G);
        #wx{obj=Win,event=#wxMouse{type=enter_window}} ->
            wxWindow:setFocus(Win), 
            loop(S, G);

	{set_click, Fun} ->
	    loop(S , G#graph{click_fun = Fun});
	
	%% percept pryl
	{set_runnability, N} ->
	    loop(S, G#graph{ runnability = runnability(N, undefined, G#graph.runnability), cr = N });


        %% Graph manipulation
	{add_event, {K1,K2}=Keys} ->
	    loop(S, G#graph{ events = gb_trees:enter(Keys, 20, G#graph.events)});
	
        {add_node, Key, State, Name} ->
	    loop(S, graph_add_node_unsure(Key, State, Name, G));
        {del_node, Key} ->
	    loop(S, graph_del_node(Key, G));
        {change_node, Key, Color} ->
	    loop(S, graph_change_node(Key, Color, G));
        {add_link, {K0,K1}, Wheel} ->
	    Tree = G#graph.es_wheel,
	    EsWheel = case gb_trees:is_defined(Wheel, Tree) of
	    	true ->
		    EsW  = graph_add_link(K0, K1, gb_trees:get(Wheel, Tree)),
		    gb_trees:enter(Wheel, EsW, Tree);
		false ->
		    EsW  = graph_add_link(K0, K1, provisual_fgraph:new()),
		    gb_trees:enter(Wheel, EsW, Tree)
	    end,
	    loop(S, G#graph{ es_wheel = EsWheel});
        {del_link, {K0,K1}, Wheel} ->
	    Tree = G#graph.es_wheel,
	    EsWheel = case gb_trees:is_defined(Wheel, Tree) of
	    	true ->
		    EsW  = graph_del_link(K0, K1, gb_trees:get(Wheel, Tree)),
		    gb_trees:enter(Wheel, EsW, Tree);
		false ->
		    Tree
	    end,
	    loop(S, G#graph{ es_wheel = EsWheel});
	
	clear_nodes_and_links ->
	    loop(S, G#graph{ 	vs = provisual_fgraph:new(), 
	    			es = default, 
				es_wheel = gb_trees:enter(default, provisual_fgraph:new(), gb_trees:empty()),
		  		events = gb_trees:empty()});

        {set_links, Wheel} ->
	    case gb_trees:is_defined(Wheel, G#graph.es_wheel) of
	    	true  -> loop(S, G#graph{ es = Wheel});
		false -> loop(S, G)
	    end;

        {Req, redraw} ->
	    {SizeX, SizeY} = wxWindow:getSize(S#state.window),
	    %io:format("redraw: get es~n"),
	    Es = gb_trees:get(G#graph.es, G#graph.es_wheel),
	    %io:format("redraw: step graph~n"),
            Vs = provisual_fgraph:step(G#graph.vs, Es, {SizeX/2.0 - 20.0, SizeY/2.0}),
            case S#state.is_frozen of
                false -> 
                    Req ! {self(), ok};
                true ->
                    ignore
            end,
	    %io:format("redraw: do redraw~n"),
            redraw(S, G),
            loop(S, G#graph{ vs = Vs, events = dec_events(G#graph.events)} );

        {stop, Reason} ->
	    unlink(S#state.parent_pid),
	    exit(Reason);

        Other ->
            error_logger:format("~p~p got unexpected message:\n\t~p\n",
                                [?MODULE, self(), Other]),          
            loop(S, G)
    %end
    end.

dec_events(Events) -> dec_events(gb_trees:to_list(Events), []).
dec_events([], Out) -> gb_trees:from_orddict(Out);
dec_events([{Keys, I}|Events], Out) when I < 1 ->
    dec_events(Events, Out);
dec_events([{Keys, I}|Events], Out) ->
    dec_events(Events, [{Keys, I - 1}|Out]).

mouse_left_down_select(G, {X0,Y0}) ->
    G#graph{ select = {{X0,Y0}, {X0,Y0}} }.

mouse_left_down_move(#graph{vs = Vs} = G, {X, Y}) ->
    % point on node?
    case coord_to_key(G, {X, Y}) of
        false ->
            G#graph{ offset_state = {X,Y}};
        {true, Key} ->
            V = #fg_v{ type = Type} = provisual_fgraph:get(Key, Vs), 
            G#graph{ vs = provisual_fgraph:set(Key, V#fg_v{ type = moving}, Vs), select = {node, Key, Type, X, Y} }
    end.

coord_to_key(#graph{vs = Vs, offset = {Xo, Yo}}, {X, Y}) ->
    Xr = X - Xo,
    Yr = Y - Yo,
    provisual_fgraph:foldl(fun({Key, #fg_v{ p = {Px, Py}}}, _) when abs(Px - Xr) < 10,
                                                          abs(Py - Yr) < 10 -> {true, Key};
                    (_, Out) -> Out
                 end, false, Vs).    

mouse_left_up_select(G, {_X,_Y}) ->
    case G#graph.select of
        {{X0,Y0}, {X1, Y1}} ->
            {Xo, Yo} = G#graph.offset,
            Xmin = lists:min([X0,X1]) - Xo,
            Xmax = lists:max([X1,X0]) - Xo,
            Ymin = lists:min([Y0,Y1]) - Yo,
            Ymax = lists:max([Y1,Y0]) - Yo,
            Vs = provisual_fgraph:map(fun
                ({Key, Value = #fg_v{ p = {Px, Py}}})
   		   when Px > Xmin, Px < Xmax, Py > Ymin, Py < Ymax ->
                    {Key, Value#fg_v{ selected = true }};
                ({Key, Value}) -> {Key, Value#fg_v{ selected = false }}
            end, G#graph.vs),
            G#graph{ select = none, vs = Vs};
        _ ->
            G#graph{ select = none}
    end.
        
mouse_left_up_move(#graph{ select = Select, vs = Vs, click_fun = Fun} = G, {X,Y}, Shift) ->
    case Select of
        {node, Key, _, X, Y} ->
            io:format("click: ~p\n", [Key]),
	    try
	    	Fun(Key)
	    catch
	    	E:C ->
		    io:format("~p:~p~n", [E,C])
	    end,
            G#graph{ select = none, offset_state = false };
        {node, Key, Type, _, _} ->
            V = provisual_fgraph:get(Key, Vs),
            Type2 =
                case Shift of
                    true -> static;
                    false -> Type
                end,
            G#graph{ select = none,
		     vs = provisual_fgraph:set(Key, V#fg_v{ type = Type2}, Vs),
		     offset_state = false };
        _ ->
            G#graph{ select = none, offset_state = false }
    end.
        
mouse_motion_select(G, {X,Y}) ->
    case G#graph.select of
        {P0, _P1} -> G#graph{ select = {P0, {X,Y}}};
        _        -> G
    end.

mouse_motion_move(G = #graph{ select = {node, Key, _, _, _}, vs = Vs}, {X,Y}) ->
    {Xo, Yo} = G#graph.offset,
    V = provisual_fgraph:get(Key, Vs),
    V2 = V#fg_v{ p = {float(X - Xo), float(Y - Yo)}},
    G#graph{ vs = provisual_fgraph:set(Key, V2, Vs) };
mouse_motion_move(G, {X,Y}) ->
    case G#graph.offset_state of
        {X1,Y1} -> 
            {X0, Y0} = G#graph.offset,
            G#graph{ offset_state = {X,Y},
		     offset = {X0 - (X1 - X), Y0 - (Y1 - Y)} };
            _ -> 
                G
    end.

%% percept pryl

runnability(N, Ts, Ra) ->
    if 
    	length(Ra) > 100 ->
	    [_|NRa] = lists:reverse(Ra),
	    [{Ts,N}|lists:reverse(NRa)];
	true ->
	    [{Ts,N}|Ra]
    end.


%% redraw etc


redraw(#state{window=Win}, G) ->
    DC0  = wxClientDC:new(Win),
    DC   = wxBufferedDC:new(DC0),
    Size = wxWindow:getSize(Win),
    redraw(DC, Size, G),
    wxBufferedDC:destroy(DC),
    wxClientDC:destroy(DC0),
    ok.

redraw(DC, _Size, G) ->    
    wx:batch(fun() -> 
        Es    = gb_trees:get(G#graph.es, G#graph.es_wheel), 
        %io:format("do redraw: es ~p~n", [G#graph.es]),
        Pen   = G#graph.pen,
        Font  = G#graph.font,
        Brush = G#graph.brush,
        wxDC:setTextForeground(DC,?color_fg),
        wxBrush:setColour(Brush, ?color_bg),
        wxDC:setBrush(DC, Brush),
        wxDC:setBackground(DC, Brush),
        wxDC:clear(DC),

	% draw events (msgs)
	Events = gb_trees:to_list(G#graph.events),
        %io:format("do redraw: events ~p~n", [length(Events)]),
	wxPen:setColour(Pen, {20,200, 200}),
        wxDC:setPen(DC,Pen),
	draw_events(DC, Pen, G#graph.vs, Events, G#graph.offset),

        % draw vertices and edges 
        wxPen:setWidth(Pen, 1),
        wxPen:setColour(Pen, ?color_fg),
        wxDC:setPen(DC,Pen),

        %draw_es(DC, G#graph.es_pts, G#graph.offset),
        draw_es(DC, G#graph.vs, Es, G#graph.offset, Pen, Brush),
        draw_vs(DC, G#graph.vs, G#graph.offset, Pen, Brush),

	%draw_runnability(DC, G#graph.runnability),

        % draw selection box
        wxPen:setColour(Pen, ?color_fg),
        wxDC:setPen(DC,Pen),
        draw_select_box(DC, G#graph.select),


        % draw information text
        wxFont:setWeight(Font,?wxNORMAL),
        draw_text(DC, provisual_fgraph:size(G#graph.vs), provisual_fgraph:size(Es), G#graph.ke, G#graph.cr),

        ok
    end).

draw_runnability(Dc, Rs) -> 
    draw_line(Dc, {20,1000}, {820,1000}),
    draw_runnability(Dc, Rs, 0).

draw_runnability(_, [], _) -> ok;
draw_runnability(Dc, [{_, N}|Rs], Xi) ->
    X = Xi*8 + 20,
    Y = 1000 - N*13,
    draw_line(Dc, {X, 1000}, {X, Y}),
    draw_runnability(Dc, Rs, Xi + 1).

draw_events(_, _, _, [], _) -> ok;
draw_events(DC, Pen, Vs, [{{From,To}, I}|Events], Po) ->
    case {provisual_fgraph:get(From, Vs), provisual_fgraph:get(To, Vs)} of
    	{undefined, undefined} -> draw_events(DC, Pen, Vs, Events, Po);
    	{_, undefined} -> draw_events(DC, Pen, Vs, Events, Po);
    	{undefined, _} -> draw_events(DC, Pen, Vs, Events, Po);
	{#fg_v{ p = P1}, #fg_v{p = P2}} ->
	    R = I/20,
	    W = round(5*R + 1),
	    wxPen:setWidth(Pen, W),
	    wxDC:setPen(DC,Pen),
	    draw_line(DC, P1, P2, Po),
	    draw_events(DC, Pen, Vs, Events, Po)
    end.
    

draw_select_box(DC, {{X0,Y0}, {X1,Y1}}) ->
    draw_line(DC, {X0,Y0}, {X1,Y0}, {0,0}),
    draw_line(DC, {X1,Y1}, {X1,Y0}, {0,0}),
    draw_line(DC, {X1,Y1}, {X0,Y1}, {0,0}),
    draw_line(DC, {X0,Y0}, {X0,Y1}, {0,0}),
    ok;
draw_select_box(_DC, _) -> 
    ok.

draw_es(DC, Vs, Es, Po, Pen, Brush) ->
    provisual_fgraph:foreach(fun
        ({{K1, K2}, _}) ->
            #fg_v{ p = P1} = provisual_fgraph:get(K1, Vs),
            #fg_v{ p = P2} = provisual_fgraph:get(K2, Vs),
            draw_arrow(DC, P1, P2, Po, Pen, Brush)
        end, Es).

draw_arrow(DC, {X0,Y0}, {X1, Y1}, {X, Y}, Pen, Brush) ->
    Xdiff = (X0 - X1) / 4,
    Ydiff = (Y0 - Y1) / 4,
    X2 = X1 + Xdiff + X,
    Y2 = Y1 + Ydiff + Y,
    wxDC:setPen(DC, Pen),
    wxDC:setBrush(DC, Brush),

    draw_line(DC, {X0,Y0}, {X1, Y1}, {X, Y}),

    %% Draw arrow head
    Radians = calc_angle({X0, Y0}, {X1, Y1}),
    Len = 10,
    %% Angle = 30,
    %% Degrees = radians_to_degrees(Radians),
    %% Radians2 = degrees_to_radians(Degrees + Angle + 180),
    %% Radians3 = degrees_to_radians(Degrees - Angle + 180),
    Radians2 = Radians + 3.665191429188092,
    Radians3 = Radians + 2.617993877991494,
    {X3, Y3} = calc_point({X2, Y2}, Len, Radians2),
    {X4, Y4} = calc_point({X2, Y2}, Len, Radians3),
    Points = [{round(X2), round(Y2)},
	      {round(X3), round(Y3)},
	      {round(X4), round(Y4)}],
    wxDC:drawPolygon(DC, Points, []).

draw_line(DC, P1, P2) -> draw_line(DC, P1, P2, {0,0}).
draw_line(DC, {X0,Y0}, {X1, Y1}, {X, Y}) ->
    wxDC:drawLine(DC, {round(X0 + X), round(Y0 + Y)}, {round(X1 + X), round(Y1 + Y)}).
   
draw_vs(DC, Vs, {Xo, Yo}, Pen, Brush) ->
    provisual_fgraph:foreach(fun
	({_Key, #fg_v{ p ={X, Y}, color = Color, name = String, selected = Sel}}) ->
	    case Sel of
		true ->
		    wxPen:setColour(Pen, ?color_fg),
		    wxBrush:setColour(Brush, ?color_bg),
		    wxDC:setPen(DC,Pen),
		    wxDC:setBrush(DC, Brush),
		    SelProps = {round(X-12 + Xo), round(Y-12 + Yo), 24, 24},
		    wxDC:drawRoundedRectangle(DC, SelProps, float(?ARC_R)),
		    ok;
		false ->
		    ok
	    end,
	    case Color of
		default -> 
		    wxPen:setColour(Pen, ?color_default),
		    wxBrush:setColour(Brush, ?color_default_bg);
		alternate -> 
		    wxPen:setColour(Pen, ?color_alternate),
		    wxBrush:setColour(Brush, ?color_alternate_bg);
		{FgColor, BgColor} ->
		    wxPen:setColour(Pen, FgColor),
		    wxBrush:setColour(Brush, BgColor);		    
		Color ->
		    wxPen:setColour(Pen, Color),
		    wxBrush:setColour(Brush, Color)
	    end,
	    wxDC:setPen(DC,Pen),
	    wxDC:setBrush(DC, Brush),
	    NodeProps = {round(X-8 + Xo),round(Y-8 + Yo),17,17},
	    wxDC:drawRoundedRectangle(DC, NodeProps, float(?ARC_R)),
	    wxDC:drawText(DC, String, {round(X + Xo), round(Y + Yo)}),
	    ok;
	(_) ->
	    ok
    end, Vs).

draw_text(DC, Nvs, Nes, _KE, Cr) ->
    VsString = "#nodes: " ++ integer_to_list(Nvs),
    EsString = "#links: " ++ integer_to_list(Nes),
    CrString = "#procs: " ++ integer_to_list(Cr),
    %% KEString = " ke: " ++ s(KE),
    wxDC:drawText(DC, VsString, {10,10}),
    wxDC:drawText(DC, EsString, {10,25}),
    wxDC:drawText(DC, CrString, {10,40}),
    %% wxDC:drawText(DC, KEString, {10,40}),
    ok.

s(Format, Terms) -> lists:flatten(io_lib:format(Format, Terms)).
s(Term) when is_float(Term) -> s("~.2f", [Term]);
s(Term) when is_integer(Term) -> integer_to_list(Term);
s(Term) when is_atom(Term) -> atom_to_list(Term);
s(Term) -> s("~p", [Term]).

%% Calclulate angle in radians for a line between two points
calc_angle({X1, Y1}, {X2, Y2}) ->
    math:atan2((Y2 - Y1), (X2 - X1)).

%% Calc new point at a given distance and angle from another point
calc_point({X, Y}, Length, Radians) ->
    X2 = round(X + Length * math:cos(Radians)),
    Y2 = round(Y + Length * math:sin(Radians)),
    {X2, Y2}.

%% %% Convert from an angle in radians to degrees
%% radians_to_degrees(Radians) ->
%%     Radians * 180 / math:pi().
%% 
%% %% Convert from an angle in degrees to radians
%% degrees_to_radians(Degrees) ->
%%     Degrees * math:pi() / 180.



