%%%-------------------------------------------------------------------
%%% @author Wen Jian
%%% @copyright (C) 2023, KTH Royal Institute of Technology
%%% @doc
%%% Assignment HW2 for ID2201 - Distributed Systems
%%% @end
%%% Created : 15. 9月 2023 18:14
%%%-------------------------------------------------------------------
-module(intf).
-author("Wen Jian").

%% API
-export([new/0, add/4, remove/2, lookup/2, ref/2, name/2, list/1, broadcast/2]).

new() ->  % Interface: the adjacent nodes that is reachable from this node.
  [].

add(Name, Ref, Pid, Intf) ->
  lists:keystore(Name, 1, Intf, {Name, Ref, Pid}).

remove(Name, Intf) ->
  lists:keydelete(Name, 1, Intf).

lookup(Name, Intf) ->
  case lists:keyfind(Name, 1, Intf) of
    {_, _, Pid} ->
      {ok, Pid};
    false ->
      notfound
  end.

ref(Name, Intf) ->
  case lists:keyfind(Name, 1, Intf) of
    {_, Ref, _} ->
      {ok, Ref};
    false ->
      notfound
  end.

name(Ref, Intf) ->
  case lists:keyfind(Ref, 2, Intf) of
    {Name, _, _} ->
      {ok, Name};
    false ->
      notfound
  end.

list(Intf) ->  % Get a list names of all the interfaces,
  lists:map(fun(Elem) -> {Name, _, _} = Elem, Name end, Intf).

broadcast(Message, Intf) ->  % Broadcast to all the interfaces.
  lists:foreach(fun({_, _, Pid}) -> Pid ! Message end, Intf).