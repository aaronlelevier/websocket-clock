-module(interact_h).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-import(ezwebframe_mochijson2, [encode/1, decode/1]).

init(Req, Opts) ->
  {cowboy_websocket, Req, Opts}.

websocket_init(State) ->
  {ok, State}.

websocket_handle({text, Msg}, State) ->
  io:format("Line:~p Msg:~p State:~p~n", [?LINE, Msg, State]),
  util:handler({text, Msg}, State, self());
websocket_handle(Msg, State) ->
  io:format("Line:~p Msg:~p State:~p~n", [?LINE, Msg, State]),
  {ok, State}.

websocket_info({Pid, {struct, [{entry, _Input}, {txt, Msg0}]}}, State) ->
  io:format("Line:~p websocket_info Pid:~p State:~p Msg0:~p~n",
    [?LINE, Pid, State, Msg0]),
  Time = clock_h:current_time(),
  Msg = [{cmd, append_div}, {id, scroll},
    {txt, erlang:list_to_binary([Time, " > ", Msg0, "<br>"])}],
  Text = list_to_binary(encode([{struct, Msg}])),
  {reply, {text, Text}, State};
websocket_info(Info, State) ->
  io:format("Line:~p websocket_info Info:~p State:~p~n",
    [?LINE, Info, State]),
  {ok, State}.
