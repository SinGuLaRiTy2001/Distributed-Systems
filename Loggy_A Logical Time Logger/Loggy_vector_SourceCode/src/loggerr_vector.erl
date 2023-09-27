%%%-------------------------------------------------------------------
%%% @author Wen Jian
%%% @copyright (C) 2023, KTH Royal Institute of Technology
%%% @doc
%%% Assignment HW3 for ID2201 - Distributed Systems
%%% @end
%%% Created : 27. 9月 2023 13:59
%%%-------------------------------------------------------------------
-module(loggerr_vector).
-author("Wen Jian").

%% API
-export([start/1, stop/1]).

start(_) ->
  spawn_link(fun() ->init() end).

stop(Logger) ->
  Logger ! stop.

init() ->
  LogClock = time_vector:clock(),
  HoldbackQueue = [],
  MaxSize = 0,
  loop(LogClock, HoldbackQueue, MaxSize).

loop(LogClock, HoldbackQueue, MaxSize) ->
  receive
    {log, From, Time, Msg} ->
      NewLogClock = time_vector:update(From, Time, LogClock),  % Update the LogClock for that node.
      NewQueue = lists:append([{From, Time, Msg}], HoldbackQueue),  % Add this log into queue.
      UnsafeQueue = queue_guard(NewQueue, NewLogClock),  % For each entry, if safe: log; if unsafe: keep in queue.
      NewMaxSize = max(MaxSize, erlang:length(UnsafeQueue)),
      % log(From, Time, Msg),
      loop(NewLogClock, UnsafeQueue, NewMaxSize);  % Wait for updates with new holdback queue.
    stop ->
      io:format("Max holdback queue length: ~w ~n", [MaxSize]),
      ok
  end.

queue_guard([], _) ->
  [];
queue_guard(Queue, LogClock) ->
  [{From, Time, Msg} | T] = Queue,
  case time_vector:safe(Time, LogClock) of
    true ->  % For safe msg, just log it.
      log(From, Time, Msg),
      queue_guard(T, LogClock);
    false ->
      lists:append(T, [{From, Time, Msg}])
  end.

log(From, Time, Msg) ->
  io:format("log: ~w ~w ~p~n", [Time, From, Msg]).
