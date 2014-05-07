-module(ludo_game_state).
-compile(export_all).

-export([add_entity/2,delete_entity/0,find_entity_by_id/2,find_entities_in_area/4]).

-include("include/records.hrl").

add_entity(State = #state{entities=Entities, entityCount=EntityCount}, Entity) ->
	ID = EntityCount,
	NewEntity = Entity#entity{id=ID},
	{ID, State#state{entities=[NewEntity | Entities], entityCount=EntityCount + 1}}.

delete_entity() ->
tbi.

find_entity_by_id(State = #state{entities=Entities}, EntityID) ->
L = lists:keyfind(EntityID,2,Entities),
case L of
	false -> not_found;
	{_,Entity} -> Entity
end.

find_entities_in_area(State = #state{entities=Entities}, X, Y, R) ->
tbi.


