%%%-------------------------------------------------------------------
%%% @author Wen Jian
%%% @copyright (C) 2023, KTH Royal Institute of Technology
%%% @doc
%%% Assignment HW5 for ID2201 - Distributed Systems
%%% @end
%%% Created : 07. 10月 2023 15:34
%%%-------------------------------------------------------------------
-module(storage).
-author("Wen Jian").

%% API
-compile(export_all).

create() ->
  [].

add(Key, Value, Store) ->
  lists:append(Store, [{Key, Value}]).

lookup(Key, Store) ->
  lists:keyfind(Key, 1, Store).

split(From, To, Store) ->
  Updated = [{Key, Value} || {Key, Value} <- Store, key:between(Key, From, To)],
  Rest = [{Key, Value} || {Key, Value} <- Store, not key:between(Key, From, To)],
  {Updated, Rest}.

merge(Entries, Store) ->
  Store ++ Entries.
