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

-module(percept_win).

-export([
	
         stop/2
        ]).

-include_lib("wx/include/wx.hrl").

-record(state,
        {
          parent_pid,
          frame,
          window,
          width,
          height,
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
          es = [],
	  wheel = [],
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
-define(l_slider, 91).
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

add_link(Pid, {FromKey, ToKey}, Wheel) -> Pid ! {add_link, {FromKey, ToKey}, Wheel}.
del_link(Pid, {FromKey, ToKey}, Wheel) -> Pid ! {del_link, {FromKey, ToKey}, Wheel}.

stop(Pid, Reason) -> 
    Ref = erlang:monitor(process, Pid),
    Pid ! {stop, Reason},
    receive
        {'DOWN', Ref, _, _, _} ->
            ok
    end.

set_dbl_click(Pid, Fun) -> Pid ! {set_dbl_click, Fun}.

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
    SL  = wxSlider:new(ParentWin, ?l_slider, ?default_l, 1, 500, [{style, ?wxVERTICAL}]),
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
    wxSizer:add(SliderSizer, SL, [{flag, ?wxEXPAND}, {proportion, 1}]),
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
    wxButton:setToolTip(SL, "Control link length."),
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
    wxWindow:connect(SL, command_slider_updated),
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
  
    Vs = fgraph:new(),
    Es = fgraph:new(),

    Me = self(),
    Ticker = spawn_link(fun() -> ticker_init(Me) end),
    
    loop( #state{ parent_pid = Pid,
                  q_slider = SQ,
		  l_slider = SL,
		  k_slider = SK,
                  mouse_act = ?move,
		  frame = ParentWin,
		  window = Win,
                  is_frozen = false,
		  ticker = Ticker},
          #graph{ vs = Vs,
		  es = Es,

		  pen = Pen,
		  font = Font,
		  events = gb_trees:empty(),
		  brush = Brush}).


