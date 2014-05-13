-module(a_star_tests).
-include_lib("eunit/include/eunit.hrl").

astar_fail_test() ->    
    Start = {1,6},
    Goal = {10,6},
    ShortestPath = [{1,6},{2,6},{3,5},{4,4},{5,3},{6,4},{7,5},{8,6},{9,6},{10,6}],
    List = sets:new(),
    ShortestPath == lists:reverse(a_star:astar(Start, Goal, map:create_map({0,0},{11,11},[{5,4},{5,5},{5,6},{5,7},{5,8}],List))).

astar1_test() ->    
    Start = {1,1},
    Goal = {5,5},
    ShortestPath = [{2,2},{3,3},{4,4},{5,5}],
    List = sets:new(),
    ShortestPath == lists:reverse(a_star:astar(Start, Goal, map:create_map({0,0},{5,5},[],List))).

astar2_test() ->    
    Start = {1,1},
    Goal = {5,5},  
    List = sets:new(),
    failure =/= lists:reverse(a_star:astar(Start, Goal, map:create_map({0,0},{5,5},[],List))).


dist_between_test() ->
    Node1 = {0,0},
    Node2 = {10,10},
    Result = 20,
    Result == a_star:dist_between(Node1,Node2).

h_score_test() ->
    Node1 = {0,0},
    Node2 = {10,10},
    Result = 200,
    Result == a_star:h_score(Node1,Node2).
