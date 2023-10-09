%%%-------------------------------------------------------------------
%%% @author Wen Jian
%%% @copyright (C) 2023, KTH Royal Institute of Technology
%%% @doc
%%% Assignment HW5 for ID2201 - Distributed Systems
%%% @end
%%% Created : 08. 10月 2023 14:53
%%%-------------------------------------------------------------------
-module(environment).
-author("Wen Jian").

%% API
-compile(export_all).

-define(Timeout, 10000).

performance1(N, Module) ->  % 4 machine perform N lookups
  N1 = test:start(Module),
  timer:sleep(1000),
  spawn(fun() -> add_lookup(1, N, N1) end),
  spawn(fun() -> add_lookup(2, N, N1) end),
  spawn(fun() -> add_lookup(3, N, N1) end),
  spawn(fun() -> add_lookup(4, N, N1) end),
  spawn(fun() -> add_lookup(5, N, N1) end),
  N1.

performance2(N, Module) ->  % 1 machine perform N lookups
  N1 = test:start(Module),
  timer:sleep(1000),
  spawn(fun() -> add_lookup(0, N, N1) end),
  N1.

add_lookup(Id, N, P) ->
  T1 = now(),
  io:format("Test Machine~w:~w add ~w elements~n", [Id, self(), N]),
  Keys = test:keys(N),
  test:add(Keys, P),
  test:check(Keys, P, 0 ,0),
  T2 = now(),
  Done = (timer:now_diff(T2, T1) div 1000),
  io:format("Test Machine~w: finish in ~w ms ~n", [Id, Done]).

hand_fail_demo() ->
  N1 = test:start(node4),
  N2 = test:start(node4, N1),
  N3 = test:start(node4, N1),
  N4 = test:start(node4, N1),
  N5 = test:start(node4, N1),
  {N1, N2, N3, N4, N5}.