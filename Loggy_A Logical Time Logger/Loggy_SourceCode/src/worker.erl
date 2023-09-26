%%%-------------------------------------------------------------------
%%% @author Wen Jian
%%% @copyright (C) 2023, KTH Royal Institute of Technology
%%% @doc
%%% Assignment HW3 for ID2201 - Distributed Systems
%%% @end
%%% Created : 22. 9月 2023 14:54
%%%-------------------------------------------------------------------
-module(worker).
-author("Wen Jian").

%% API
-export([start/5, stop/1, peers/2]).

start(Name, Logger, Seed, Sleep, Jitter) ->
  spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).

stop(Worker) ->
  Worker ! stop.

init(Name, Log, Seed, Sleep, Jitter) ->
  % rand:seed(Seed, Seed, Seed),
  rand:seed(exs1024s,[Seed, Seed, Seed]),
  receive
    {peers, Peers} ->
      LocalTime = time:zero(),  % Start working by initialize the timer as 0
      loop(Name, Log, Peers, Sleep, Jitter, LocalTime);
    stop ->
      ok
  end.

peers(Wrk, Peers) ->
  Wrk ! {peers, Peers}.

loop(Name, Log, Peers, Sleep, Jitter, LocalTime)->
  Wait = rand:uniform(Sleep),
  receive
    {msg, Time, Msg} ->
      NewTime = time:inc(Name, time:merge(LocalTime, Time)),  % Once receive, update local timer.
      Log ! {log, Name, NewTime, {received, Msg}},
      loop(Name, Log, Peers, Sleep, Jitter, NewTime);
    stop ->
      ok;
    Error ->
      Log ! {log, Name, time, {error, Error}}
  after Wait ->
    Selected = select(Peers),
    NewTime = time:inc(Name, LocalTime),  % When sending, update local timer as well.
    Message = {hello, rand:uniform(100)},
    Selected ! {msg, NewTime, Message},  % Send msg to randomly picked up peer from given group
    jitter(Jitter),
    Log ! {log, Name, NewTime, {sending, Message}},
    loop(Name, Log, Peers, Sleep, Jitter, NewTime)
  end.

select(Peers) ->
  lists:nth(rand:uniform(length(Peers)), Peers).

jitter(0) ->
  ok;
jitter(Jitter) ->
  timer:sleep(rand:uniform(Jitter)).
