-module(ludo_game_pathfinding).
-compile(export_all).

%% @doc A* algorithm, searches for the optimal path to take between two tiles

astar(Start,Goal,ClosedList) ->

    Closedset = ClosedList,  % The set of nodes already evaluated.
    Openset = sets:add_element(Start, sets:new()), %The set of tentative nodes to be evaluated
    
    Fscore = dict:append(Start, h_score(Start, Goal), dict:new()),
    Gscore = dict:append(Start, 0, dict:new()), % Distance from start along optimal path.
    
    CameFrom = dict:append(Start, none, dict:new()),
    
    lists:reverse(astar_step(Goal, Closedset, Openset, Fscore, Gscore, CameFrom)).

%% @doc Take the most optimal step

astar_step(Goal, Closedset, Openset, Fscore, Gscore, CameFrom) ->
    case sets:size(Openset) of
	0 ->
	    [];
	_ ->
	    case best_step(sets:to_list(Openset), Fscore, none, infinity) of
		Goal ->
		    reconstruct_path(CameFrom, Goal);
		X ->
		    NextOpen = sets:del_element(X, Openset),
		    NextClosed = sets:add_element(X, Closedset),
		    Neighbours = neighbour_nodes(X),
		    {NewOpen, NewF, NewG, NewFrom} = scan(X, Neighbours, NextOpen, NextClosed, Fscore, Gscore, CameFrom, Goal),    
		    astar_step(Goal, NextClosed, NewOpen, NewF, NewG, NewFrom)
	    end
    end.

%% @doc Adds all availible tiles to a openset

scan(_X, [], Open, _Closed, F, G, From, _Goal) ->
    {Open, F, G, From};
scan(X, [Y|N], Open, Closed, F, G, From, Goal) ->
    case sets:is_element(Y, Closed) of
	true ->
	    scan(X, N, Open, Closed, F, G, From, Goal);
	false ->
	    [G0] = dict:fetch(X, G),
	    TrialG = G0 + dist_between(X,Y),
	    case sets:is_element(Y, Open) of
		true ->
		    [OldG] = dict:fetch(Y, G),
		    case TrialG < OldG of
			true ->
			    {NewF, NewG, NewFrom} = update(X, Y, F, G, From, TrialG, Goal),
			    scan(X, N, Open, Closed, NewF, NewG, NewFrom, Goal);
			false ->
			    scan(X, N, Open, Closed, F, G, From, Goal)
		    end;
		false ->
		    NewOpen = sets:add_element(Y, Open),
		    {NewF, NewG, NewFrom} = update(X, Y, F, G, From, TrialG, Goal),
		    scan(X, N, NewOpen, Closed, NewF, NewG, NewFrom, Goal)
	    end
    end.

%% @doc Update the optimal path

update(X, Y, OldF, OldG, OldFrom, GValue, Goal) ->
    
    KeyF = dict:is_key(Y, OldF),
    KeyG = dict:is_key(Y, OldG),  
    KeyFrom = dict:is_key(Y, OldFrom),
    case {KeyF, KeyG, KeyFrom} of
	{true, _, _} ->
	    update(X, Y, dict:erase(Y, OldF), OldG, OldFrom, GValue, Goal);
	{_, true, _} ->
	    update(X, Y, OldF, dict:erase(Y, OldG), OldFrom, GValue, Goal);
	{_, _, true} ->
	    update(X, Y, OldF, OldG, dict:erase(Y, OldFrom), GValue, Goal);
	_ ->
	    NewFrom = dict:append(Y, X, OldFrom),
	    NewG = dict:append(Y, GValue, OldG),
	    NewF = dict:append(Y, GValue + h_score(Y,Goal), OldF), % Estimated total distance from start to goal through y.
	    {NewF, NewG, NewFrom}
    end.

%% @doc Reconstruct the path from current tile back to the start tile

reconstruct_path(CameFrom, Node) ->
    case dict:fetch(Node, CameFrom) of
	[none] ->
	    [Node];
	[Value] ->
	    [Node | reconstruct_path(CameFrom, Value)]
    end.


%% @doc Searches for the lowest fscore in openset

best_step([H|Open], Score, none, _) ->
    [V] = dict:fetch(H, Score),
    best_step(Open, Score, H, V);
best_step([], _Score, Best, _BestValue) ->
    Best;
best_step([H|Open], Score, Best, BestValue) ->
    [Value] = dict:fetch(H, Score),
    case Value < BestValue of
	true ->
	    best_step(Open, Score, H, Value);
	false ->
	    best_step(Open, Score, Best, BestValue)
    end.

%% @doc Creates a list of all the neighbouring tiles

neighbour_nodes({X,Y}) ->
    NorthTile = {X,(Y+1)},
    NorthEastTile = {(X+1),(Y+1)},
    EastTile = {(X+1),Y},
    SouthEastTile = {(X+1),(Y-1)},
    SouthTile = {X,(Y-1)},
    SouthWestTile = {(X-1),(Y-1)},
    WestTile = {(X-1),Y},
    NorthWestTile = {(X-1),(Y+1)},
    [NorthTile,NorthEastTile,EastTile,SouthEastTile,SouthTile,SouthWestTile,WestTile,NorthWestTile].

%% @doc Returns the distance between two tiles 

dist_between({X1,Y1},{X2,Y2}) ->
    (abs((X2-X1))+abs((Y2-Y1))).

%% @doc Returns the score between two tiles 

h_score(ThisTile, Goal) ->
    (dist_between(ThisTile, Goal)*10).


