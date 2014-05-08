-module(ludo_game_state).
-compile(export_all).

-export([add_entity/2,delete_entity/2,find_entity_by_id/2,find_entities_in_area/4,get_free_entity_id/1]).

-include("include/records.hrl").

add_entity(State = #state{entities=Entities, entityCount=EntityCount}, Entity) ->
	ID = get_free_entity_id(State),
	NewEntity = Entity#entity{id=ID},
	{ID, State#state{entities=[NewEntity | Entities], entityCount=EntityCount + 1}}.

delete_entity(State = #state{entities=Entities, entityCount=EntityCount}, Entity) ->
	NewEntities = [E || E <- Entities, E =/= Entity],
	State#state{entities=NewEntities,entityCount=EntityCount - 1}.

find_entity_by_id(#state{entities=Entities}, EntityID) ->
	L = [Entity || Entity = #entity{id=ID} <- Entities, ID == EntityID],
	case L of
		[] -> not_found;
		[Entity] -> Entity
	end.

get_free_entity_id(#state{entities=[]}) ->
	0;
get_free_entity_id(#state{entities=Entities}) ->
	LastEntity = lists:last(Entities),
	LastEntityID = LastEntity#entity.id,
	FreeEntityID = LastEntityID+1,
	FreeEntityID.

find_entities_in_area(_State = #state{entities=_Entities}, _X, _Y, _R) ->
	tbi.


