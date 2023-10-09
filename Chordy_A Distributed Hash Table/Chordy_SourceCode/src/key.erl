%%%-------------------------------------------------------------------
%%% @author Wen Jian
%%% @copyright (C) 2023, KTH Royal Institute of Technology
%%% @doc
%%% Assignment HW5 for ID2201 - Distributed Systems
%%% @end
%%% Created : 07. 10月 2023 13:38
%%%-------------------------------------------------------------------
-module(key).
-author("Wen Jian").

%% API
-export([generate/0, between/3]).

generate() ->
  rand:uniform(1000000000).

between(Key, From, To) ->
  if
    From == To ->
      true;
    (From < Key) and (Key =< To) and (From < To) ->
      true;
    ((From < Key) or (Key =< To)) and (From > To) ->
      true;
    true ->
      false
  end.
