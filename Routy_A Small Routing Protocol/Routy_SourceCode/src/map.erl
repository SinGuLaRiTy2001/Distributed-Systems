%%%-------------------------------------------------------------------
%%% @author Wen Jian
%%% @copyright (C) 2023, KTH Royal Institute of Technology
%%% @doc
%%% Assignment HW2 for ID2201 - Distributed Systems
%%% @end
%%% Created : 15. 9月 2023 12:32
%%%-------------------------------------------------------------------
-module(map).
-author("Wen Jian").

%% API
-export([new/0, update/3, reachable/2, all_nodes/1]).

new() ->
  [].

update(Node, Links, Map) ->
  lists:keystore(Node, 1, Map, {Node, Links}).  % Store a set of new links (neighbors) for Node in the map

reachable(Node, Map) ->
  case lists:keyfind(Node, 1, Map) of  % Find the sets for Node in the map
    {_, Links} ->  % If found, return the set of links (neighbors)
      Links;
    false ->  % If not found, return an empty set
      []
  end.

all_nodes(Map) ->
  lists:foldl(fun(Elem, AccIn) -> accumulator(Elem, AccIn) end, [], Map).
  % Use lists:foldl as accumulator to get all the elements

accumulator({Node, Links}, Acc) ->
  EntryList = [Elem || Elem <- [Node | Links], not lists:member(Elem, Acc)],
  % Use list comprehension to remove duplicate entry
  lists:append(EntryList, Acc).