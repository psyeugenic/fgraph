-module(test).
-export([start/0, do/0, do/1]).

-include("wx.hrl").
-include("fgraph.hrl").

-record(state,{
	frame,
	window,
	graphl,
	graphr
	}).



start() ->
    spawn(fun() -> init() end).


do() -> do(9).
do(N) when N > 1 ->
    Vs = lists:seq(1,N),
    R  = trunc(math:sqrt(N)),
%    lists:flatten(lists:foldl(fun
%    	(I, O) when I rem R > 0 -> 
%	    [{I, I + 1}|O];
%	(_, O) ->
%	    O
%	end, [], lists:seq(1, N))) ++
    Es1 = 
        [{I, I + 1} || I <- lists:seq(1, N - 1), (I rem R) > 0] ++ 
	[{Ri + R*Ci, Ri + R + R*Ci} || Ri <- lists:seq(1, R), Ci <- lists:seq(0, R - 2)],
	
%    Es1 = [ 
%	{1,2}, {2,3},
%	{1,4}, {2,5}, {3,6},
%	{4,5}, {5,6},
%	{4,7}, {5,8}, {6,9},
%	{7,8}, {8,9}
%	],
    Es2 = [{I, I+1} || I <- lists:seq(1,N - 1)] ++ [{1,N}],

    lists:foreach(fun
	(V) -> fgraph_panel:add_node(left_panel, V)
    end, Vs),
    lists:foreach(fun
	(V) -> fgraph_panel:add_node(right_panel, V)
    end, Vs),
    lists:foreach(fun
	(E) -> fgraph_panel:add_edge(left_panel, E)
    end, Es1),
    lists:foreach(fun
	(E) -> fgraph_panel:add_edge(right_panel, E)
    end, Es2),

    ok.
    

init() ->
    Wt = 1200,
    Ht = 1000,
    Wx=wx:new(),
    Frame = wxFrame:new(Wx, -1, "Test", []),
    
    wxFrame:createStatusBar(Frame,[]),
    wxFrame:connect(Frame, close_window),

    WindowSizer = wxBoxSizer:new(?wxHORIZONTAL),
    Panel = wxPanel:new(Frame),

    %% ADD 2 x fgraph_window    

    {LPid, FgraphL} = fgraph_panel:new(Panel),
    {RPid, FgraphR} = fgraph_panel:new(Panel),

    register(left_panel, LPid),
    register(right_panel, RPid),

    SF = wxSizerFlags:new(),
    wxSizerFlags:proportion(SF,2),
    wxSizer:add(WindowSizer, FgraphL, wxSizerFlags:proportion(wxSizerFlags:expand(SF),1)), 
    wxSizer:add(WindowSizer, FgraphR, wxSizerFlags:proportion(wxSizerFlags:expand(SF),1)), 
 
    wxWindow:setSizer(Panel,WindowSizer),
    wxSizer:fit(WindowSizer, Frame),
    wxSizer:setSizeHints(WindowSizer,Frame),
    wxWindow:setSizeHints(Frame, {Wt, Ht}),

    wxWindow:show(Frame),

    loop(#state{ frame = Frame, graphl = LPid, graphr = RPid}).


loop(S) ->
    receive
	M -> 
	    io:format("~p~n", [M]), 
	    loop(S)
    end.
