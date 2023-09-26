%%%-------------------------------------------------------------------
%%% @author Wen Jian
%%% @copyright (C) 2023, KTH Royal Institute of Technology
%%% @doc
%%% Assignment HW3 for ID2201 - Distributed Systems
%%% @end
%%% Created : 22. 9月 2023 15:05
%%%-------------------------------------------------------------------
-module(test).
-author("Wen Jian").

%% API
-export([run/2, run/6]).

run(Sleep, Jitter) -> 
  run(Sleep, Jitter, 13, 23, 36, 49).

run(Sleep, Jitter, Seed1, Seed2, Seed3, Seed4) ->
  Log = loggerr:start([john, paul, ringo, george]),
  A = worker:start(john, Log, Seed1, Sleep, Jitter),
  B = worker:start(paul, Log, Seed2, Sleep, Jitter),
  C = worker:start(ringo, Log, Seed3, Sleep, Jitter),
  D = worker:start(george, Log, Seed4, Sleep, Jitter),
  worker:peers(A, [B, C, D]),
  worker:peers(B, [A, C, D]),
  worker:peers(C, [A, B, D]),
  worker:peers(D, [A, B, C]),
  timer:sleep(5000),
  loggerr:stop(Log),
  worker:stop(A),
  worker:stop(B),
  worker:stop(C),
  worker:stop(D).