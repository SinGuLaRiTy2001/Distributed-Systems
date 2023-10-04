%%%-------------------------------------------------------------------
%%% @author Wen Jian
%%% @copyright (C) 2023, KTH Royal Institute of Technology
%%% @doc
%%% Assignment HW4 for ID2201 - Distributed Systems
%%% @end
%%% Created : 01. 10月 2023 11:06
%%%-------------------------------------------------------------------
-module(gms1).
-author("Wen Jian").

%% API
-compile([export_all]).

-define(arghh, 100000).

leader(Id, Master, Slaves, Group) ->
  receive  % Message from master or slaves.
    {mcast, Msg} ->  % Receive multicast request.
      io:format("Leader:~w bcast message: ~w to ~w~n", [Id, Msg, Slaves]),
      bcast(Id, {msg, Msg}, Slaves),  % Multicast.
      Master ! Msg,  % Send to application layer.
      leader(Id, Master, Slaves, Group);
    {join, Wrk, Peer} ->  % Receive join request.
      io:format("leader ~w receive join request from its master_pid~w~n", [Id,Master]),
      NewSlaves = lists:append(Slaves, [Peer]),  % Add new node.
      NewGroup = lists:append(Group, [Wrk]),  % Add new group.
      io:format("leader ~w announce: New member join!~nNewGroup ~w~nNewSlave~w~n", [Id, NewGroup,NewSlaves]),
      Master ! {view, NewGroup, [self()|NewSlaves]},  % Update to application layer.
%%      io:format("leader ~w bcast new view to all slaves~n", [Id]),
      bcast(Id, {view, [self()|NewSlaves], NewGroup}, NewSlaves),  % Broadcast to peers.
      leader(Id, Master, NewSlaves, NewGroup);
    stop ->
      ok
  end.

slave(Id, Master, Leader, Slaves, Group) ->
  receive  % For all msgs, pass to leader.
    {mcast, Msg} ->  % Receive multicast request.
      Leader ! {mcast, Msg},
      slave(Id, Master, Leader, Slaves, Group);
    {join, Wrk, Peer} ->  % Receive join request.
      Leader ! {join, Wrk, Peer},
      slave(Id, Master, Leader, Slaves, Group);
    {msg, Msg} ->  % Msg from leader, send to application layer.
      Master ! Msg,
      slave(Id, Master, Leader, Slaves, Group);
    {view, [Leader|NewSlaves], NewGroup} ->  % View from leader, send to application layer.
      Master ! {view, NewGroup},
      slave(Id, Master, Leader, NewSlaves, NewGroup);
    {state_request, Msg} ->
%%      io:format("slave ~w receive a state_request from his master~n", [Id]),
%%      io:format("slave ~w send this state_request to leader~n", [Id]),
      Leader ! {mcast, Msg},
      slave(Id, Master, Leader, Slaves, Group);
    stop ->
      ok
  end.

% Initialize first node as leader.
start(Id) ->
  Self = self(),
  {ok, spawn_link(fun()->init(Id, Self) end)}.

init(Id, Master) ->
  io:format("slave ~w is the leader~n", [Id]),
  leader(Id, Master, [], [Master]).

% Initialize subsequent nodes as slaves.
start(Id, Group) ->
  Self = self(),
  {ok, spawn_link(fun()->init(Id, Group, Self) end)}.

init(Id, Group, Master) ->
  Self = self(),
  io:format("slave ~w send join to worker_pid:~w ~n", [Id, Group]),
  Group ! {join, Master, Self, Id},
  io:format("slave ~w wait for view ~n", [Id]),
  receive
    {view, [Leader|NewSlaves], NewGroup} ->  % View from leader, send to application layer.
      io:format("slave ~w: got view ~n", [Id]),
      Master ! {view, NewGroup},
      io:format("slave ~w: send view to its master~n", [Id]),
      slave(Id, Master, Leader, NewSlaves, NewGroup)
  end.

bcast(_, Msg, Nodes) ->
  lists:foreach(fun(Node)-> Node ! Msg end, Nodes).
