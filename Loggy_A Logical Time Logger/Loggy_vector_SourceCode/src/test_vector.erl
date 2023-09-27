%%%-------------------------------------------------------------------
%%% @author Wen Jian
%%% @copyright (C) 2023, KTH Royal Institute of Technology
%%% @doc
%%% Assignment HW3 for ID2201 - Distributed Systems
%%% @end
%%% Created : 27. 9月 2023 15:29
%%%-------------------------------------------------------------------
-module(test_vector).
-author("Wen Jian").

%% API
-export([run/2, run/6]).

run(Sleep, Jitter) ->
  run(Sleep, Jitter, 13, 23, 36, 49).

run(Sleep, Jitter, Seed1, Seed2, Seed3, Seed4) ->
  Log = loggerr_vector:start([john, paul, ringo, george]),
  A = worker_vector:start(john, Log, Seed1, Sleep, Jitter),
  B = worker_vector:start(paul, Log, Seed2, Sleep, Jitter),
  C = worker_vector:start(ringo, Log, Seed3, Sleep, Jitter),
  D = worker_vector:start(george, Log, Seed4, Sleep, Jitter),
  worker_vector:peers(A, [B, C, D]),
  worker_vector:peers(B, [A, C, D]),
  worker_vector:peers(C, [A, B, D]),
  worker_vector:peers(D, [A, B, C]),
  timer:sleep(5000),
  loggerr_vector:stop(Log),
  worker_vector:stop(A),
  worker_vector:stop(B),
  worker_vector:stop(C),
  worker_vector:stop(D).
