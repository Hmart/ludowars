-module(ludo_game_server).
-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(ParentPID) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	{ok, {{one_for_one, 60, 3600},
		[]}}.