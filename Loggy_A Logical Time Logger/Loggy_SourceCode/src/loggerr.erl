%%%-------------------------------------------------------------------
%%% @author Wen Jian
%%% @copyright (C) 2023, KTH Royal Institute of Technology
%%% @doc
%%% Assignment HW3 for ID2201 - Distributed Systems
%%% @end
%%% Created : 22. 9月 2023 14:43
%%%-------------------------------------------------------------------
-module(loggerr).
-author("Wen Jian").

%% API
-export([start/1, stop/1]).

start(Nodes) ->
  spawn_link(fun() ->init(Nodes) end).

stop(Logger) ->
  Logger ! stop.

init(Nodes) ->
  LogClock = time:clock(Nodes),
  HoldbackQueue = [],
  MaxSize = 0,
  loop(LogClock, HoldbackQueue, MaxSize).

loop(LogClock, HoldbackQueue, MaxSize) ->
  receive
    {log, From, Time, Msg} ->
      NewLogClock = time:update(From, Time, LogClock),  % Update the LogClock for that node.
      NewQueue = lists:append(HoldbackQueue, [{From, Time, Msg}]),  % Add this log into queue.
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
  SortedQueue = lists:keysort(2, Queue),
  [{From, Time, Msg} | T] = SortedQueue,
  case time:safe(Time, LogClock) of
    true ->  % For safe msg, just log it.
      log(From, Time, Msg),
      queue_guard(T, LogClock);
    false ->
      SortedQueue  % The log left are unsafe (beyond LogClock).
  end.

log(From, Time, Msg) ->
  io:format("log: ~w ~w ~p~n", [Time, From, Msg]).
