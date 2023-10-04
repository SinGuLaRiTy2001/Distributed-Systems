%%%-------------------------------------------------------------------
%%% @author Wen Jian
%%% @copyright (C) 2023, KTH Royal Institute of Technology
%%% @doc
%%% Assignment HW4 for ID2201 - Distributed Systems
%%% @end
%%% Created : 01. 10月 2023 12:08
%%%-------------------------------------------------------------------
-module(gms2).
-author("Wen Jian").

%% API
-compile([export_all]).

-define(timeout,100000).
-define(arghh, 200).

leader(Id, Master, Slaves, Group) ->
  receive
    {mcast, Msg} ->
      io:format("leader:~w bcast message: ~w to ~w~n", [Id, Msg, Slaves]),
      bcast(Id, {msg, Msg}, Slaves),
      Master ! Msg,
      leader(Id, Master, Slaves, Group);
    {join, Wrk, Peer} ->
      io:format("leader ~w receive join request from its master_pid~w~n", [Id,Master]),
      NewSlaves = lists:append(Slaves, [Peer]),
      NewGroup = lists:append(Group, [Wrk]),
      io:format("leader ~w announce: New member join!~nNewGroup ~w~nNewSlave~w~n", [Id, NewGroup,NewSlaves]),
      Master ! {view, NewGroup},
%%      io:format("leader ~w bcast new view to all slaves~n", [Id]),
      bcast(Id,{view, [self()|NewSlaves], NewGroup}, NewSlaves),
      leader(Id, Master, NewSlaves, NewGroup);
    stop ->
      ok
  end.

slave(Id, Master, Leader, Slaves, Group) ->
  receive
    {mcast, Msg} ->
      Leader ! {mcast, Msg},
      slave(Id, Master, Leader, Slaves, Group);
    {join, Wrk, Peer} ->
      Leader ! {join, Wrk, Peer},
      slave(Id, Master, Leader, Slaves, Group);
    {msg, Msg} ->
      Master ! Msg,
      slave(Id, Master, Leader, Slaves, Group);
    {view, [Leader|NewSlaves], NewGroup} ->
      Master ! {view, NewGroup},
      slave(Id, Master, Leader, NewSlaves, NewGroup);
    {state_request, Msg} ->
%%      io:format("slave ~w receive a state_request from his master~n", [Id]),
%%      io:format("slave ~w send this state_request to leader~n", [Id]),
      Leader ! {mcast, Msg},
      slave(Id, Master, Leader, Slaves, Group);
    {'DOWN', _Ref, process, Leader, _Reason} ->  % If leader crushed, start new election.
      election(Id, Master, Slaves, Group);
    stop ->
      ok
  end.

start(Id) ->
  Rnd = rand:uniform(1000),
  Self = self(),
  {ok, spawn_link(fun()->init(Id, Rnd, Self) end)}.

init(Id, Rnd, Master) ->
  rand:seed(exs1024s, [Rnd, Rnd, Rnd]),
  io:format("slave ~w is the leader~n", [Id]),
  leader(Id, Master, [], [Master]).

start(Id, Group) ->
  Rnd = rand:uniform(1000),
  Self = self(),
  {ok, spawn_link(fun() -> init(Id, Group, Self, Rnd) end)}.

init(Id, Group, Master, Rnd) ->
  rand:seed(exs1024s, [Rnd, Rnd, Rnd]),
  Self = self(),
  io:format("slave ~w send join to worker_pid:~w ~n", [Id, Group]),
  Group ! {join, Master, Self, Id},
  io:format("slave ~w wait for view ~n", [Id]),
  receive
    {view, [Leader|NewSlaves], NewGroup} ->
      io:format("slave ~w: got view ~n", [Id]),
      Master ! {view, NewGroup},
      io:format("slave ~w: send view to its master~n", [Id]),
      erlang:monitor(process, Leader),  % Track DOWN from crushed leader.
      slave(Id, Master, Leader, NewSlaves, NewGroup)
  after ?timeout ->
    Master ! {error, "no reply from leader!"}
  end.

bcast(Id, Msg, Nodes) ->
  lists:foreach(fun(Node) -> Node ! Msg, crash(Id) end, Nodes).

crash(Id) ->
  case rand:uniform(?arghh) of
    ?arghh ->
      io:format("leader ~w: crash ~n", [Id]),
      exit(no_luck);
    _ ->
      ok
  end.

election(Id, Master, Slaves, [_|NewGroup]) ->  % [_|NewGroup]: New group without former leader.
  Self = self(),
  case Slaves of
    [Self|Rest] ->  % Leader is current node -> Broadcast.
      bcast(Id,{view, [Self|Rest], NewGroup}, Rest),
      Master ! {view, NewGroup},
      io:format("leader ~w: This is leader. ~n", [Id]),
      leader(Id, Master, Rest, NewGroup);
    [Leader|Rest] ->  % Leader is another node -> Wait for it.
      erlang:monitor(process, Leader),
      io:format("slave ~w: This is slave. ~n", [Id]),
      slave(Id, Master, Leader, Rest, NewGroup)
  end.
