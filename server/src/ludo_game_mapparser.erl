-module(ludo_game_mapparser).
-compile(export_all).

%% @doc Creates the outer lined colliders 

create_borders({X1,Y1}, {X2,Y2}, Closedset) ->
    case X1 < X2 of
	true ->
	    create_borders({(X1+1),Y1},{X2,Y2}, (add_collider({X1,Y2}, (add_collider({X1,Y1}, Closedset)))));
	false ->
	    case Y1 < Y2 of
		true ->
		    create_borders({X1,(Y1+1)},{X2,Y2}, (add_collider({X2,Y1}, (add_collider({0,Y1}, Closedset)))));
		false ->
		    add_collider({X2,Y1}, (add_collider({0,Y1}, Closedset)))
	    end
    end.

%% @doc Adds a collider 

add_collider(Tile, ClosedList) -> 
    sets:add_element(Tile, ClosedList).

%% @doc Adds a list including colliders 

add_colliderList([], List) ->
    List;
add_colliderList([H|Rlist], List) ->
    add_colliderList(Rlist, add_collider(H,List)).
    
%% @doc Creates all the colliders on the map

create_map(SouthWestCorner, NorthEastCorner, ColliderList, MapList) ->
    add_colliderList(ColliderList, create_borders(SouthWestCorner, NorthEastCorner, MapList)).

%% @doc Returns the colliders of the map in a list 

searchlist([]) ->
    false;
searchlist([{struct,X}|Rest])->
    
    case <<"ServerColliders">> == proplists:get_value(<<"name">>, X) of
	true -> 
	    proplists:get_value(<<"data">>, X);
	false ->
	    searchlist(Rest)
    end.

%% @doc Returns the server colliders based on what map to use

jsonParser() ->
    {struct, Jsondata } = mochijson2:decode(test_mochijson2:readlines("../client/assets/maps/forest.json")),
    Layers = proplists:get_value(<<"layers">>, Jsondata),
    searchlist(Layers).

%% @doc Convert a tile number to a coordinate on the map

coordinateConverter(Count) ->
    Y = (Count div 40),
    X = (Count rem 40),
    {X,Y}.

%% @doc Adds all colliders to the map and saves them in a closedlist 

make_map([], ClosedList, _Count) ->
    ClosedList;
make_map([H|Rest], ClosedList, Count) ->
    case H =/= 0 of 
	true ->
	    make_map(Rest, add_collider(coordinateConverter(Count), ClosedList), (Count+1));
	false ->
	    make_map(Rest, ClosedList, (Count+1))
    end.


main()->
     make_map(jsonParser(),sets:new(),0).

