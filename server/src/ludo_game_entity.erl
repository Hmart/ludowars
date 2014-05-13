-module(ludo_game_entity).
-behaviour(gen_server).

-export([start_link/1, delete_entity/1, update_entity/1]). %% API.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]). %% gen_server.

-include("include/records.hrl").


%% Public API.
start_link(Entity) ->
	gen_server:start_link(?MODULE, [Entity], []).

delete_entity(EntityPID) ->
	gen_server:call(EntityPID,delete_entity).

update_entity(EntityPID) ->
	gen_server:call(EntityPID,update_entity).

%% gen_server.
init([Entity]) ->
	{ok, Entity}.

handle_call(delete_entity, _From, State = #entity{id=ID, statePID=StatePID}) ->
	ludo_game_state:delete_entity(StatePID, ID),
	gen_server:call(self(),stop),
	{reply, ok, State};

handle_call(update_entity, _From, State = #entity{statePID=StatePID}) ->
	ludo_game_state:update_entity(StatePID, State),
	{reply, ok, State};

handle_call(stop, _From, State) ->
	{stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.