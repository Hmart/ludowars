-module(ludo_proto).
-compile(export_all).

parseEntity(0,_,Acc) ->
    Acc;

parseEntity(EntityCount, EntityList, Acc) ->
    <<ID:(8*4), ControllerClassName:(8*256), RepresentationClassName:(8*256), DriverClassName:(8*256), X:(8*4)/float, Y:(8*4)/float, VelocityX:(8*4)/float, VelocityY:(8*4)/float, Angle:(8*4)/float, Width:(8*4), Height:(8*4), EntityRest/bits>> = EntityList, 
    EntityElement = {ID, ControllerClassName, RepresentationClassName, DriverClassName, X, Y, VelocityX, VelocityY, Angle, Width, Height},  
    parseEntity(EntityCount-1, EntityRest, [EntityElement|Acc]).
 

parser(<<PacketID:(8*1)/signed, Payload/bits>>) ->
    parser(PacketID, Payload).

parser(1, Payload)->
    <<EntityID:(8*4), X:(8*4)/float, Y:(8*4)/float, North:(8*1), South:(8*1), West:(8*1), East:(8*1), Fire:(8*1), Secondary:(8*1), MouseX:(8*4)/float, MouseY:(8*4)/float>> = Payload,
    {movePacket, EntityID, X, Y, North, South, West, East, Fire, Secondary, MouseX, MouseY};

parser(2, Payload) ->
    <<EntityID:(8*4)>> = Payload,
    {assignEntityPacket, EntityID};

parser(3, Payload) -> 
    <<X:(8*4)/float, Y:(8*4)/float, Width:(8*4)/float, Height:(8*4)/float, EntityCount:(8*2), EntityList/bits>> = Payload,
    {statePacket, X, Y, Width, Height, parseEntity(EntityCount, EntityList, [])}. 


composeEntity([],Acc)->
    Acc;

composeEntity([{ID, ControllerClassName, RepresentationClassName, DriverClassName, X, Y, VelocityX, VelocityY, Angle, Width, Height} | EntityList], Acc) ->
    ControllerClassNameString = list_to_binary(string:left(ControllerClassName, 256, $0)),
    RepresentationClassNameString = list_to_binary(string:left(RepresentationClassName, 256, $0)),
    DriverClassNameString = list_to_binary(string:left(DriverClassName, 256, $0)),
    EntityElement =  <<ID:(8*4), ControllerClassNameString/binary, RepresentationClassNameString/binary, DriverClassNameString/binary, X:(8*4)/float, Y:(8*4)/float, VelocityX:(8*4)/float, VelocityY:(8*4)/float, Angle:(8*4)/float, Width:(8*4), Height:(8*4)>>,
    composeEntity(EntityList, [EntityElement|Acc]).

composePacket(ID, Payload) ->
    BinaryPayload = iolist_to_binary(Payload),
    PayloadSize = round(bit_size(BinaryPayload) / 8),
    io:format("PayloadSize: ~p~n", [PayloadSize]),
    [<<ID:8, PayloadSize:(8*4)>>, Payload].

compose({movePacket, EntityID, X, Y, North, South, West, East, Fire, Secondary, MouseX, MouseY}) ->
    composePacket(1, <<EntityID:(8*4), X:(8*4)/float, Y:(8*4)/float, North:(8*1), South:(8*1), West:(8*1), East:(8*1), Fire:(8*1), Secondary:(8*1), MouseX:(8*4)/float, MouseY:(8*4)/float>>);

compose({assignEntityPacket, EntityID}) ->
    composePacket(2, <<EntityID:(8*4)>>);

compose({statePacket, X, Y, Width, Height, EntityList}) ->
    Temp = composeEntity(EntityList,[]),
    composePacket(3, [<<X:(8*4)/float, Y:(8*4)/float, Width:(8*4)/float, Height:(8*4)/float, (length(Temp)):(8*2)>>, Temp]).

testparser1()->
    Payload1 = <<1:(8*4), 2:(8*4)/float, 3:(8*4)/float, 4:(8*1), 5:(8*1), 6:(8*1), 7:(8*1), 8:(8*1), 9:(8*1), 10:(8*4)/float, 11:(8*4)/float>>,
    io:format("~p ~n", [parser(1, Payload1)]),
    Payload1 == compose(parser(1, Payload1)).
    
testparser2() ->
    Payload2 = <<1:(8*4)>>,
    io:format("~p ~n", [parser(2, Payload2)]),
    Payload2 == compose(parser(2, Payload2)).
    
testparser3() ->
    Payload3 = <<1:(8*16), 0:(8*2) >>,
    io:format("~p ~n", [Payload3]),
    io:format("~p ~n", [iolist_to_binary(compose(parser(3, Payload3)))]),
    Payload3 == iolist_to_binary(compose(parser(3, Payload3))).
	






       

