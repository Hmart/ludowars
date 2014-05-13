-module(map).
-export([create_map/4]).

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

add_collider(Tile, ClosedList) -> 
    sets:add_element(Tile, ClosedList).

add_colliderList([], List) ->
    List;
add_colliderList([H|Rlist], List) ->
    add_colliderList(Rlist, add_collider(H,List)).
    
create_map(SouthWestCorner, NorthEastCorner, ColliderList, MapList) ->
    add_colliderList(ColliderList, create_borders(SouthWestCorner, NorthEastCorner, MapList)).

