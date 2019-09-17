%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Sep 2019 6:35 AM
%%%-------------------------------------------------------------------
-module(clock_h).
-author("aaron lelevier").
-export([init/2, start/1, current_time/0,
  websocket_init/1,
  websocket_info/2,
  websocket_handle/2,
  websocket_handle/3,
  websocket_terminate/3,
  websocket_info/3
]).
-import(ezwebframe_mochijson2, [encode/1, decode/1]).


% placeholder `init` for Makefile

%% cowboy/examples/websocket

init(Req, _Opts) ->
  Pid = spawn_link(?MODULE, start, [self()]),
  {cowboy_websocket, Req, Pid}.

%% book-code

start(Browser) ->
  io:format("Line:~p start Browser:~p~n", [?LINE, Browser]),
  Browser ! [{cmd, fill_div}, {id, clock}, {txt, current_time()}],
  running(Browser).

running(Browser) ->
  io:format("Line:~p running Browser:~p~n", [?LINE, Browser]),
  receive
    {Browser, {struct, [{clicked, <<"stop">>}]}} ->
      Browser ! [{cmd, fill_div}, {id, clock}, {txt, <<"Stopped">>}],
      idle(Browser);
    Other ->
      io:format("Line:~p running Other:~p~n", [?LINE, Other]),
      running(Browser)
  after 1000 ->
    Browser ! [{cmd, fill_div}, {id, clock}, {txt, current_time()}],
    running(Browser)
  end.

idle(Browser) ->
  io:format("Line~p idle~n", [?LINE]),
  receive
    {Browser, {struct, [{clicked, <<"start">>}]}} ->
      Browser ! [{cmd, fill_div}, {id, clock}, {txt, <<"Starting">>}],
      running(Browser);
    Other ->
      io:format("Line:~p idle Other:~p~n", [?LINE, Other]),
      running(Browser)
  end.

current_time() ->
  {Hour, Min, Sec} = time(),
  FTime = io_lib:format("~2.2.0w:~2.2.0w:~2.2.0w", [Hour, Min, Sec]),
  io:format("Line:~p clock_h:current_time:~p~n", [?LINE, FTime]),
  list_to_binary(FTime).

%% helpers

log_line(Line) ->
  io:format("Line:~p~n", [Line]).

%%----------------------------------------------------------------------
%% websocket stuff

%% aaron

%% called when the the websocket connection is first established
%% State is the client Pid
-spec websocket_init(State::pid()) -> tuple().
websocket_init(State) ->
  io:format("Line:~p websocket_init State:~p~n", [?LINE, State]),
  Msg = [{cmd, fill_div}, {id, clock}, {txt, current_time()}],
  Bin = list_to_binary(encode([{struct, Msg}])),
  {reply, {text, Bin}, State}.

websocket_handle({text, Msg}, State) ->
  io:format("Line:~p Msg:~p State:~p~n", [?LINE, Msg, State]),
  websocket_handle({text, Msg}, State, self()).

%% TODO: 2nd func clause works on Browswer click, but start/stop loop not working

websocket_info({log, Text}, State) ->
  io:format("Line:~p websocket_info~n", [?LINE]),
  {reply, {text, Text}, State};
websocket_info({Pid, {struct, [{clicked, Msg0}]}}, State) ->
  io:format("Line:~p websocket_info Pid:~p State:~p Msg0:~p~n",
    [?LINE, Pid, State, Msg0]),
  Msg = [{cmd, fill_div}, {id, clock}, {txt, current_time()}],
  B = list_to_binary(encode([{struct, Msg}])),
  {reply, {text, B}, State};
websocket_info(Info, State) ->
  io:format("Line:~p websocket_info Info:~p State:~p~n", [?LINE, Info, State]),
  {ok, State}.

%% book code

websocket_handle({text, Msg}, Req, Pid) ->
  io:format("Line:~p Msg:~p Req:~p Pid:~p~n", [?LINE, Msg, Req, Pid]),
  %% This is a Json message from the browser
  case catch decode(Msg) of
    {'EXIT', _Why} ->
      Pid ! {invalidMessageNotJSON, Msg};
    {struct, _} = Z ->
      io:format("Line:~p Z:~p~n", [?LINE, Z]),
      X1 = atomize(Z),
      io:format("Line:~p X1:~p~n", [?LINE, X1]),
      Pid ! {self(), X1};
    Other ->
      io:format("Line:~p Other:~p~n", [?LINE, Other]),
      Pid ! {invalidMessageNotStruct, Other}
  end,
  {ok, Req}.

websocket_info({send, Str}, Req, Pid) ->
  log_line(?LINE),
  {reply, {text, Str}, Req, Pid, hibernate};
websocket_info([{cmd, _} | _] = L, Req, Pid) ->
  log_line(?LINE),
  B = list_to_binary(encode([{struct, L}])),
  {reply, {text, B}, Req, Pid, hibernate};
websocket_info(Info, Req, Pid) ->
  log_line(?LINE),
  io:format("Handle_info Info:~p Pid:~p~n", [Info, Pid]),
  {ok, Req, Pid, hibernate}.

websocket_terminate(_Reason, _Req, Pid) ->
  log_line(?LINE),
  io:format("websocket.erl terminate:~n"),
  exit(Pid, socketClosed),
  ok.

%%----------------------------------------------------------------------
%% atomize turns all the keys in a struct to atoms

atomize({struct,L}) ->
  {struct, [{binary_to_atom(I), atomize(J)} || {I, J} <- L]};
atomize(L) when is_list(L) ->
  [atomize(I) || I <- L];
atomize(X) ->
  X.

binary_to_atom(B) ->
  list_to_atom(binary_to_list(B)).