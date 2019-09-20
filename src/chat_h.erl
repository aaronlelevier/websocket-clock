-module(chat_h).

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
  clock_h:handler({text, Msg}, State, self());
websocket_handle(Msg, State) ->
  io:format("Line:~p Msg:~p State:~p~n", [?LINE, Msg, State]),
  {ok, State}.

websocket_info({Pid, {struct, [{join, Name}]}}, State) ->
  io:format("Line:~p websocket_info Pid:~p Name:~p State:~p~n",
    [?LINE, Pid, Name, State]),
  Msg = [{cmd, append_div}, {id, scroll},
    {txt, erlang:list_to_binary([Name, " joined the group<br>"])}],
  Text = list_to_binary(encode([{struct, Msg}])),
  % send cmd to append to "users" div
  erlang:start_timer(0, self(), Name),
  % sends "joined" Msg
  {reply, {text, Text}, State};
websocket_info({timeout, _Ref, Name}, State) ->
  io:format("Line:~p websocket_info Name:~p State:~p~n",
    [?LINE, Name, State]),
  Msg = [{cmd, fill_div}, {id, users},
    {txt, erlang:list_to_binary([" > ", Name, "<br>"])}],
  Text = list_to_binary(encode([{struct, Msg}])),
  {reply, {text, Text}, State};
websocket_info(Info, State) ->
  io:format("Line:~p websocket_info Info:~p State:~p~n",
    [?LINE, Info, State]),
  {ok, State}.
