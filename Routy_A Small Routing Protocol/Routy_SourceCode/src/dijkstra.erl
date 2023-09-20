%%%-------------------------------------------------------------------
%%% @author Wen Jian
%%% @copyright (C) 2023, KTH Royal Institute of Technology
%%% @doc
%%% Assignment HW2 for ID2201 - Distributed Systems
%%% @end
%%% Created : 15. 9月 2023 13:49
%%%-------------------------------------------------------------------
-module(dijkstra).
-author("Wen Jian").

%% API
-export([update/4, iterate/3, table/2, route/2]).

entry(Node, Sorted) ->
  case lists:keyfind(Node, 1, Sorted) of  % Find the distance to Node in the Sorted list
    {_, Len, _} ->
      Len;
    false ->  % If not found, return 0
      0
  end.

replace(Node, N, Gateway, Sorted) ->  % Replace the information of Node in the Sorted list
  Entry = {Node, N, Gateway},
  UnSorted = lists:keyreplace(Node, 1, Sorted, Entry),  % After the replace, the list is unsorted
  lists:sort(fun(Entry1, Entry2) -> cmp(Entry1, Entry2) end, UnSorted).  % Sort with cmp

cmp({_, N1, _}, {_, N2, _}) ->
  if
    N1 < N2 ->
      true;
    true ->
      false
  end.

update(Node, N, Gateway, Sorted) ->
  PreLen = entry(Node, Sorted),  % Get previous distance from the Sorted list
  if
    PreLen > N ->
      replace(Node, N, Gateway, Sorted);  % If the new distance is shorter, update
    true ->
      Sorted  % Otherwise, use the old list.
  end.

iterate([], _, Table) ->
  Table;
iterate([{_, inf, _} | _], _, Table) ->
  Table;
iterate([{Node, N, Gateway} | RestSorted], Map, Table) ->
  case map:reachable(Node, Map) of  % Retrieve all the adjacent nodes (neighbors) from map
    [] ->  % If there is nothing, go on with the rest nodes
      iterate(RestSorted, Map, [{Node, Gateway} | Table]);
    Reachable ->  % For each adjacent nodes, use itself update the rest nodes
      NewSorted = lists:foldl(fun(Elem, AccIn) -> update(Elem, N+1, Gateway, AccIn) end, RestSorted, Reachable),
      iterate(NewSorted, Map, [{Node, Gateway} | Table])  % Go on with the rest nodes
  end.

table(Gateways, Map) ->  % Initialize a new table and iterate.
  IniList = lists:map(fun(Elem) -> {Elem, inf, unknown} end, map:all_nodes(Map)),
  IniSorted = lists:foldl(fun(Elem, AccIn) -> update(Elem, 0, Elem, AccIn) end, IniList, Gateways),
  iterate(IniSorted, Map, []).

route(Node, Table) ->  % Find through table to find the next node.
  case lists:keyfind(Node, 1, Table) of
    {Node, Gateway} ->
      {ok, Gateway};
    false ->
      notfound
  end.