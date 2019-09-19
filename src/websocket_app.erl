%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(websocket_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.
start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			% chat w/ reply
			{"/", cowboy_static, {priv_file, websocket, "index.html"}},
			{"/websocket", ws_h, []},
			% clock
			{"/clock", cowboy_static, {priv_file, websocket, "clock.html"}},
			{"/websocket/clock", clock_h, []},
			% interact
			{"/interact", cowboy_static, {priv_file, websocket, "interact.html"}},
			{"/websocket/interact", interact_h, []},
			% static assets
			{"/static/[...]", cowboy_static, {priv_dir, websocket, "static"}}
		]}
	]),
	{ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
		env => #{dispatch => Dispatch}
	}),
	websocket_sup:start_link().

stop(_State) ->
	ok.
