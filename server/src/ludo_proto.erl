-module(ludo_proto).
-compile(export_all).

-include("include/records.hrl").

parseEntity(0,_,Acc) ->
    Acc;

parseEntity(EntityCount, EntityList, Acc) ->
    <<ID:(8*4), ControllerClassName:(8*256), RepresentationClassName:(8*256), DriverClassName:(8*256), X:(8*4)/float, Y:(8*4)/float, VelocityX:(8*4)/float, VelocityY:(8*4)/float, Angle:(8*4)/float, Width:(8*4), Height:(8*4), EntityRest/bits>> = EntityList, 
    EntityElement = #entity{
        id = ID, 
        controller = ControllerClassName, 
        representation = RepresentationClassName, 
        driver = DriverClassName, 
        positionX = X, 
        positionY = Y, 
        velocityX = VelocityX, 
        velocityY = VelocityY, 
        angle = Angle, 
        width = Width, 
        height = Height 
    },
    parseEntity(EntityCount-1, EntityRest, [EntityElement|Acc]).

parse(<<PacketID:(8*1)/signed, _Length:(8*4), Payload/bits>>) ->
    parse(PacketID, Payload).

parse(1, Payload)->
    <<
        EntityID:(8*4), 
        X:(8*4)/float, 
        Y:(8*4)/float, 
        North:(8*1), 
        South:(8*1), 
        West:(8*1), 
        East:(8*1), 
        Fire:(8*1), 
        Secondary:(8*1), 
        MouseX:(8*4)/float, 
        MouseY:(8*4)/float
    >> = Payload,
    {move_packet, #driverState{
        entityID=EntityID,
        positionX=X,
        positionY=Y,
        north=North,
        south=South,
        west=West,
        east=East,
        fire=Fire,
        secondary=Secondary,
        mouseX=MouseX,
        mouseY=MouseY
    }};

parse(2, Payload) ->
    <<EntityID:(8*4)>> = Payload,
    {assign_entity_packet, {EntityID}};

parse(7, Payload) ->
    <<_Length:(8*2), Text/binary>> = Payload,
    {chat_packet, {binary_to_list(Text)}}.

composeEntity(#entity{
        id = ID, 
        controller = ControllerClassName, 
        representation = RepresentationClassName, 
        driver = DriverClassName, 
        positionX = X, 
        positionY = Y, 
        velocityX = VelocityX, 
        velocityY = VelocityY, 
        angle = Angle, 
        width = Width, 
        height = Height}) ->
    ControllerClassNameString = list_to_binary(string:left(ControllerClassName, 256, $\0)),
    RepresentationClassNameString = list_to_binary(string:left(RepresentationClassName, 256, $\0)),
    DriverClassNameString = list_to_binary(string:left(DriverClassName, 256, $\0)),
    <<ID:(8*4), ControllerClassNameString/binary, RepresentationClassNameString/binary, DriverClassNameString/binary, X:(8*4)/float, Y:(8*4)/float, VelocityX:(8*4)/float, VelocityY:(8*4)/float, Angle:(8*4)/float, Width:(8*4), Height:(8*4)>>.

composeEntityList(Entities) ->
    lists:map(fun(X) -> composeEntity(X) end, Entities).

compose(ID, Payload) ->
    BinaryPayload = iolist_to_binary(Payload),
    PayloadSize = round(bit_size(BinaryPayload) / 8),
    %%io:format("PayloadSize: ~p~n", [PayloadSize]),
    Packet = [<<ID:8, PayloadSize:(8*4)>>, Payload],
    %%io:format("PacketData: ~p~n", [iolist_to_binary(Packet)]),
    Packet.

compose({move_packet, #driverState{
        entityID=EntityID,
        positionX=X,
        positionY=Y,
        north=North,
        south=South,
        west=West,
        east=East,
        fire=Fire,
        secondary=Secondary,
        mouseX=MouseX,
        mouseY=MouseY
    }}) ->
    compose(1, <<EntityID:(8*4), X:(8*4)/float, Y:(8*4)/float, North:(8*1), South:(8*1), West:(8*1), East:(8*1), Fire:(8*1), Secondary:(8*1), MouseX:(8*4)/float, MouseY:(8*4)/float>>);

compose({assign_entity_packet, {EntityID}}) ->
    compose(2, <<EntityID:(8*4)>>);

compose({add_entity, {EntityData}}) ->
    compose(4, composeEntity(EntityData));

compose({delete_entity, {EntityID}}) ->
    compose(5, <<EntityID:(8*4)>>);

compose({update_entity, {EntityData}}) ->
    compose(6, composeEntity(EntityData));

compose({chat_packet, {Text}}) ->
    Length = string:len(Text),
    BinaryText = list_to_binary(Text),
    compose(7, <<Length:(8*2), BinaryText/binary>>);

compose({state_packet, {#state{
        worldBoundsX = X,
        worldBoundsY = Y,
        worldBoundsWidth = Width,
        worldBoundsHeight = Height,
        entities = Entities}}}) ->
    Temp = composeEntityList(Entities),
    compose(3, [<<X:(8*4)/float, Y:(8*4)/float, Width:(8*4)/float, Height:(8*4)/float, (length(Temp)):(8*2)>>, Temp]).
