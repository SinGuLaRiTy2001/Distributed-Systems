%%%-------------------------------------------------------------------
%%% @author Wen Jian
%%% @copyright (C) 2023, KTH Royal Institute of Technology
%%% @doc
%%% Assignment HW3 for ID2201 - Distributed Systems
%%% @end
%%% Created : 22. 9月 2023 15:07
%%%-------------------------------------------------------------------
-module(time).
-author("Wen Jian").

%% API
-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2]).

zero() ->
  0.

inc(Name, T) ->
  T + 1.

merge(Ti, Tj) ->
  max(Ti, Tj).

leq(Ti,Tj) ->
  if
    Ti =< Tj -> true;
    true -> false
  end.

clock(Nodes) ->
  initClock(Nodes, []).  % Create clock for each node.

initClock([], Clock) ->
  Clock;
initClock([Node | RestNode], Clock) ->
  NewClock = lists:append(Clock, [{Node, 0}]),  % For each node, the new clock start at 0.
  initClock(RestNode, NewClock).

update(Node, Time, Clock) ->
  lists:keyreplace(Node, 1, Clock, {Node, Time}).  % Update the clock to Time.

safe(Time, Clock) ->
  TempClock = lists:keysort(2, Clock),
  [{_, PreTime} | _] = TempClock,
  leq(Time, PreTime + 1).  % Since the PreTime is increased by 1, the safe msg: Time <= PreTime + 1.
