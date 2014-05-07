{application, ludowars, [
	{vsn, "0.1.0"},
	{registered, [ludo]},
	{mod, {ludo_app, []}},
	{env, [{port, 7331}]},
	{modules, [
		ludo_app, 
		ludo_game_server, 
		ludo_game_sup, 
		ludo_sup, 
		ludo_proto,
		ludo_game_connection_sup,
		ludo_game_connection,
		ludo_master
	]}
]}.
