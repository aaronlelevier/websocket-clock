{application, 'websocket', [
	{description, "Cowboy Websocket example"},
	{vsn, "1"},
	{modules, ['chat_h','clock_h','ezwebframe_mochijson2','interact_h','websocket_app','websocket_sup','ws_h']},
	{registered, [websocket_sup]},
	{applications, [kernel,stdlib,cowboy]},
	{mod, {websocket_app, []}},
	{env, []}
]}.