-module(ludo_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	{ok, {{one_for_one, 60, 3600},
		[{ludo_master,
		 {ludo_master, start_link, []},
		 transient, infinity, worker, [ludo_master]},
		{ludo_game_sup,
		 {ludo_game_sup, start_link, []},
		 transient, infinity, supervisor, [ludo_game_sup]}
	]}}.
