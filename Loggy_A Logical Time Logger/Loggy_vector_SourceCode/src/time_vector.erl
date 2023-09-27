%%%-------------------------------------------------------------------
%%% @author Wen Jian
%%% @copyright (C) 2023, KTH Royal Institute of Technology
%%% @doc
%%% Assignment HW3 for ID2201 - Distributed Systems
%%% @end
%%% Created : 27. 9月 2023 14:10
%%%-------------------------------------------------------------------
-module(time_vector).
-author("Wen Jian").

%% API
-export([zero/0, zero/1, inc/2, merge/2, leq/2, clock/0, update/3, safe/2]).

zero() ->
  [].
zero(Nodes) ->
  [{X, 0} || X <- Nodes].

inc(Name, T) ->
  case lists:keyfind(Name, 1, T) of
    {Name, Ti} ->
      lists:keyreplace(Name, 1, T, {Name, Ti + 1});
    false ->
      [{Name, 0} | T]
  end.

merge([], T) ->
  T;
merge([{Name, Ti} | Rest], T) ->
  case lists:keyfind(Name, 1, T) of
    {Name, Tj} ->
      [{Name, max(Ti, Tj)} | merge(Rest, lists:keydelete(Name, 1, T))];
    false ->
      [{Name, Ti} | merge(Rest, T)]
  end.

leq([], _) ->
  true;
leq([{Name, Ti} | Rest], T) ->
  case lists:keyfind(Name, 1, T) of
    {Name, Tj} ->
      if
        Ti =< Tj ->
          leq(Rest, lists:keydelete(Name, 1, T));
        true ->
          false
      end;
    false ->
      false
  end.

clock() ->
  [].

update(Node, Time, Clock) ->
  {Name, Ti} = lists:keyfind(Node, 1, Time),
  case lists:keyfind(Node, 1, Clock) of
    {Node, _} ->
      lists:keyreplace(Node, 1, Clock, {Name, Ti});
    false ->
      [{Name, Ti} | Clock]
  end.

safe(Time, Clock) ->
  leq(Time, Clock).
