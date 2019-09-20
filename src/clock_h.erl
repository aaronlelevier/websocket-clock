%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc API Server where the Client can start and stop a clock by
%%% by sending commands to the Server using websocket communication
%%% @end
%%% Created : 16. Sep 2019 6:35 AM
%%%-------------------------------------------------------------------
-module(clock_h).
-author("aaron lelevier").
-export([
  init/2,
  current_time/0,
  websocket_init/1,
  websocket_info/2,
  websocket_handle/2
]).
-import(ezwebframe_mochijson2, [encode/1, decode/1]).
-import(util, [handler/3, binary_to_atom/1]).

%% called when Cowboy starts, says to use websocket communication
%% port # to use is configured in `websocket_app.erl`
init(Req, Opts) ->
  io:format("Line:~p Req:~p Opts:~p~n", [?LINE, Req, Opts]),
  {cowboy_websocket, Req, Opts}.

%% called when 1st websocket communication is received
websocket_init(State) ->
  io:format("Line:~p websocket_init State:~p~n", [?LINE, State]),
  erlang:start_timer(100, self(), encode_and_fill_div(<<"Starting">>)),
  start_clock(),
  {ok, State}.

%% sends replies to client with:
%%  `{reply, {text, Msg}, State}`
%% results in no reply to client
%%  `{ok, State}`
%% triggered on timeout or messages to our self, for example
%% from `handler/3` below
websocket_info({timeout, _Ref, Msg}, State) ->
  io:format("Line:~p websocket_info Msg:~p State:~p~n",
    [?LINE, Msg, State]),
  case get(is_running) of
    true ->
      current_time_delay(),
      {reply, {text, Msg}, State};
    false ->
      Msg1 = encode_and_fill_div(<<"Stopped">>),
      {reply, {text, Msg1}, State}
  end;
websocket_info({Pid, {struct, [{clicked, Msg0}]}}, State) ->
  io:format("Line:~p websocket_info Pid:~p State:~p Msg0:~p~n",
    [?LINE, Pid, State, Msg0]),
  case start_or_stop_clock(Msg0) of
    true ->
      % required or else won't go in first pattern match
      % `websocket_info({timeout, ...`
      current_time_delay(),
      {reply, {text, current_time_bin()}, State};
    false ->
      {ok, State}
  end;
websocket_info(Info, State) ->
  io:format("Line:~p websocket_info Info:~p State:~p~n", [?LINE, Info, State]),
  {ok, State}.

%% handler for incoming messages from client
websocket_handle({text, Msg}, State) ->
  io:format("Line:~p Msg:~p State:~p~n", [?LINE, Msg, State]),
  handler({text, Msg}, State, self());
websocket_handle(Msg, State) ->
  io:format("Line:~p Msg:~p State:~p~n", [?LINE, Msg, State]),
  {ok, State}.

%% current time

current_time_bin() ->
  encode_and_fill_div(current_time()).

current_time() ->
  {Hour, Min, Sec} = time(),
  FTime = io_lib:format("~2.2.0w:~2.2.0w:~2.2.0w", [Hour, Min, Sec]),
  io:format("Line:~p current_time:~p~n", [?LINE, FTime]),
  list_to_binary(FTime).

current_time_delay() ->
  erlang:start_timer(1000, self(), current_time_bin()).

%% clock

start_or_stop_clock(Bin) ->
  io:format("Line:~p start_or_stop_clock Bin:~p~n", [?LINE, Bin]),
  case binary_to_atom(Bin) of
    start ->
      io:format("Line:~p start_or_stop_clock start~n", [?LINE]),
      start_clock(),
      true;
    stop ->
      io:format("Line:~p start_or_stop_clock stop~n", [?LINE]),
      stop_clock(),
      false
  end.

start_clock() ->
  put(is_running, true).

stop_clock() ->
  put(is_running, false).

%% other helpers

encode_and_fill_div(Term) ->
  Msg = [{cmd, fill_div}, {id, clock}, {txt, Term}],
  list_to_binary(encode([{struct, Msg}])).

