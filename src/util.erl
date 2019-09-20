%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Sep 2019 6:38 AM
%%%-------------------------------------------------------------------
-module(util).
-author("aaron lelevier").
-export([handler/3, atomize/1, binary_to_atom/1]).

-import(ezwebframe_mochijson2, [encode/1, decode/1]).

%% receives a binary argument `Msg` and converts it to a Term
%% then uses `Pid`, which is the Pid of the current process
%% to reply to our self and `websocket_info` will then be triggered
handler({text, Msg}, State, Pid) ->
  %% This is a Json message from the browser
  io:format("Line:~p Msg:~p State:~p Pid:~p~n", [?LINE, Msg, State, Pid]),
  case catch decode(Msg) of
    {'EXIT', _Why} ->
      Pid ! {invalidMessageNotJSON, Msg};
    {struct, _} = Z ->
      X1 = atomize(Z),
      Pid ! {self(), X1};
    Other ->
      Pid ! {invalidMessageNotStruct, Other}
  end,
  {ok, State}.

%% book-code helpers
%% `L` is a 2 item tuple of binary types, and `atomize` converts the 1st
%% item from a `binary` to an `atom`.
%% Also done recursively on the 2nd item, if it is also a list of 2 item tuples
atomize({struct, L}) ->
  {struct, [{binary_to_atom(I), atomize(J)} || {I, J} <- L]};
atomize(L) when is_list(L) ->
  [atomize(I) || I <- L];
atomize(X) ->
  X.

binary_to_atom(B) ->
  list_to_atom(binary_to_list(B)).