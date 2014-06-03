-module (ludo_proto_tests).
-include_lib("eunit/include/eunit.hrl").

createDummyData(X, Count) ->
    iolist_to_binary([<<X:(8*1)>> || _ <- lists:seq(1,Count)]).

createDummyEntity(ID) ->
    iolist_to_binary([<<ID:(8*4)>>,
     createDummyData($a, 128), createDummyData($\0, 128),
     createDummyData($a, 128), createDummyData($\0, 128),
     createDummyData($a, 128), createDummyData($\0, 128),
     createDummyData(0, 32)]).


createDummyEntityList(0)->
    [];

createDummyEntityList(Count) ->
    [createDummyEntity(Count)| createDummyEntityList(Count-1)].
    

packet1_test()->
    Payload1 = <<1:(8*4), 2:(8*4)/float, 3:(8*4)/float, 4:(8*1), 5:(8*1), 6:(8*1), 7:(8*1), 8:(8*1), 9:(8*1), 10:(8*4)/float, 11:(8*4)/float>>,
    Payload1 == ludo_proto:compose(ludo_proto:parser(1, Payload1)).
    
packet2_test() ->
    Payload2 = <<1:(8*4)>>,
    Payload2 == ludo_proto:compose(ludo_proto:parser(2, Payload2)).
    
packet3_1_test() ->
    X = 0,
    Payload5=  [<<3:(8*1), 818:(8*4), 1:(8*16), X:(8*2)>>, 
		iolist_to_binary(createDummyEntityList(X))],
    BinaryPayload5 = iolist_to_binary(Payload5),
    Payload5 == iolist_to_binary(ludo_proto:compose(ludo_proto:parser(BinaryPayload5))).
		
packet3_2_test() ->
    X = 10,
    Payload5=  [<<3:(8*1), 818:(8*4), 1:(8*16), X:(8*2)>>, 
		iolist_to_binary(createDummyEntityList(X))],
    BinaryPayload5 = iolist_to_binary(Payload5),
    Payload5 == iolist_to_binary(ludo_proto:compose(ludo_proto:parser(BinaryPayload5))).
	
packet3_3_test() ->
    X = 100,
    Payload5=  [<<3:(8*1), 818:(8*4), 1:(8*16), X:(8*2)>>, 
		iolist_to_binary(createDummyEntityList(X))],
    BinaryPayload5 = iolist_to_binary(Payload5),
    Payload5 == iolist_to_binary(ludo_proto:compose(ludo_proto:parser(BinaryPayload5))).
	
