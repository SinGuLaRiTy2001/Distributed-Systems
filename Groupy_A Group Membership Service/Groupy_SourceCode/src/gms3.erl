%%%-------------------------------------------------------------------
%%% @author Wen Jian
%%% @copyright (C) 2023, KTH Royal Institute of Technology
%%% @doc
%%% Assignment HW4 for ID2201 - Distributed Systems
%%% @end
%%% Created : 01. 10月 2023 16:08
%%%-------------------------------------------------------------------
-module(gms3).
-author("Wen Jian").

%% API
-compile([export_all]).

-define(timeout,100000).
-define(arghh, 200).

leader(Id, Master, N, Slaves, Group)->
  receive
    {mcast, Msg}->
      io:format("~w leader:~w bcast message ~w: ~w to ~w~n", [self(),Id, N, Msg, Slaves]),
      bcast(Id, {msg, N, Msg}, Slaves),
      Master ! Msg,
      leader(Id, Master, N+1, Slaves, Group);
    {join, Wrk, Peer} ->
      io:format("leader ~w receive join request from its master_pid~w~n", [Id,Master]),
      NewSlaves = lists:append(Slaves, [Peer]),
      NewGroup = lists:append(Group, [Wrk]),
      io:format("leader ~w announce: New member join!~nNewGroup ~w~nNewSlave~w~n", [Id, NewGroup,NewSlaves]),
      Master ! {view, NewGroup},
%%      io:format("leader ~w bcast new view to all slaves~n", [Id]),
      bcast(Id, {view, N, [self()|NewSlaves], NewGroup}, NewSlaves),
      leader(Id, Master, N+1, NewSlaves, NewGroup);
    stop ->
      ok
  end.

slave(Id, Master, Leader, N, Last, Slaves, Group)->
  receive
    {mcast, Msg}->
      Leader ! {mcast, Msg},
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    {join, Wrk, Peer} ->
      Leader ! {join, Wrk, Peer},
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    {msg, I, _} when I < N ->  % Discard msg that I < N.
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    {msg, N, Msg} ->
      NewLast = {msg, N, Msg},
      Master ! Msg,
      slave(Id, Master, Leader, N + 1, NewLast, Slaves, Group);
    {view, I, _, _} when I < N ->  % Discard msg that I < N.
      io:format("~w", I),
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    {view, N, [Leader|NewSlaves], NewGroup}->
      NewLast = {view, N, [Leader|NewSlaves], NewGroup},
      Master ! {view, NewGroup},
      slave(Id, Master, Leader, N + 1, NewLast, NewSlaves, NewGroup);
    {state_request, Msg}->
%%      io:format("slave ~w receive a state_request from his master~n", [Id]),
%%      io:format("slave ~w send this state_request to leader~n", [Id]),
      Leader ! {mcast, Msg},
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    {'DOWN', _Ref, process, Leader, _Reason} ->
      election(Id, Master, N, Last, Slaves, Group);
    stop ->
      ok
  end.

start(Id)->
  Rnd = rand:uniform(1000),
  Self = self(),
  {ok, spawn_link(fun()->init(Id, Rnd, Self) end)}.

init(Id, Rnd, Master)->
  rand:seed(exs1024s, [Rnd, Rnd, Rnd]),
  io:format("slave ~w is the leader~n", [Id]),
  leader(Id, Master, 0, [], [Master]).

start(Id, Group)->
  Rnd = rand:uniform(1000),
  Self = self(),
  {ok, spawn_link(fun()->init(Id, Group, Self, Rnd) end)}.

init(Id, Group, Master, Rnd)->
  rand:seed(exs1024s, [Rnd, Rnd, Rnd]),
  Self = self(),
  io:format("slave ~w send join to worker_pid:~w ~n", [Id, Group]),
  Group ! {join, Master, Self, Id},
  io:format("slave ~w wait for view ~n", [Id]),
  receive
    {view, N, [Leader|NewSlaves], NewGroup}->
      Last = {view, N, [Leader|NewSlaves], NewGroup},
      io:format("slave ~w: got view ~n", [Id]),
      Master ! {view, NewGroup},
      io:format("slave ~w: send view to its master~n", [Id]),
      erlang:monitor(process, Leader),
      slave(Id, Master, Leader, N+1, Last, NewSlaves, NewGroup)
  after ?timeout->
    Master ! {error, "no reply from leader!"}
  end.

bcast(Id, Msg, Nodes)->
  lists:foreach(fun(Node)-> Node ! Msg, crash(Id) end, Nodes).

crash(Id)->
  case rand:uniform(?arghh) of
    ?arghh ->
      io:format("leader ~w: crash ~n", [Id]),
      exit(no_luck);
    _->
      ok
  end.

election(Id, Master, N, Last, Slaves, [_|NewGroup])->
  Self = self(),
  case Slaves of
    [Self|Rest]->
      bcast(Id, Last, Rest),
      bcast(Id, {view, N,[Self|Rest], NewGroup}, Rest),
      Master ! {view, NewGroup},
      io:format("~wleader ~w: This is leader. ~nNew group: ~w~n", [self(), Id, NewGroup]),
      leader(Id, Master, N+1, Rest, NewGroup);
    [Leader|Rest]->
      erlang:monitor(process, Leader),
      io:format("~wslave ~w: This is slave. ~n", [self(), Id]),
      slave(Id, Master, Leader, N, Last, Rest, NewGroup)
  end.
