%%%-------------------------------------------------------------------
%%% @author Wen Jian
%%% @copyright (C) 2023, KTH Royal Institute of Technology
%%% @doc
%%% Assignment HW5 for ID2201 - Distributed Systems
%%% @end
%%% Created : 07. 10月 2023 15:53
%%%-------------------------------------------------------------------
-module(node2).
-author("Wen Jian").

%% API
-compile([export_all]).

-define(Stabilize, 100).
-define(Timeout, 10000).

node(Id, Predecessor, Successor, Store) ->
  receive
    {key, Qref, Peer} ->
      Peer ! {Qref, Id},
      node(Id, Predecessor, Successor, Store);
    {notify, New} ->
      {Pred, KeptStore} = notify(New, Id, Predecessor, Store),
      node(Id, Pred, Successor, KeptStore);
    {request, Peer} ->
      request(Peer, Predecessor),
      node(Id, Predecessor, Successor, Store);
    {status, Pred} ->
      Succ = stabilize(Pred, Id, Successor),
      node(Id, Predecessor, Succ, Store);
    stabilize ->
      stabilize(Successor),
      node(Id, Predecessor, Successor, Store);
    probe ->
      create_probe(Id, Successor, Store),
      node(Id, Predecessor, Successor, Store);
    {probe, Id, Nodes, T} ->
      remove_probe(T, Nodes),
      node(Id, Predecessor, Successor, Store);
    {probe, Ref, Nodes, T} ->
      forward_probe(Ref, T, Nodes, Id, Successor, Store),
      node(Id, Predecessor, Successor, Store);
    {add, Key, Value, Qref, Client} ->
      Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Added);
    {lookup, Key, Qref, Client} ->
      lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Store);
    {handover, Elements} ->
      Merged = storage:merge(Store, Elements),
      node(Id, Predecessor, Successor, Merged)
  end.

start(Id) ->
  start(Id, nil).
start(Id, Peer) ->
  timer:start(),
  spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
  Predecessor = nil,
  {ok, Successor} = connect(Id, Peer),
  schedule_stabilize(),
  node(Id, Predecessor, Successor, storage:create()).

connect(Id, nil) ->
  {ok, {Id, self()}};
connect(_, Peer) ->
  Qref = make_ref(),
  Peer ! {key, Qref, self()},
  receive
    {Qref, Skey} ->
      {ok, {Skey,Peer}}
  after ?Timeout ->
    io:format("Time out.~n")
  end.

schedule_stabilize()->
  timer:send_interval(?Stabilize, self(), stabilize).

stabilize({_, Spid}) ->
  Spid ! {request, self()}.

request(Peer, Predecessor) ->
  case Predecessor of
    nil ->
      Peer ! {status, nil};
    {Pkey, Ppid} ->
      Peer ! {status, {Pkey, Ppid}}
  end.

notify({Nkey, Npid}, Id, Predecessor, Store) ->
  case Predecessor of
    nil ->
      Keep = handover(Id, Store, Nkey, Npid),
      {{Nkey, Npid}, Keep};
    {Pkey, _} ->
      case key:between(Nkey, Pkey, Id) of
        true ->
          Keep = handover(Id, Store, Nkey, Npid),
          {{Nkey,Npid}, Keep};
        false ->
          {Predecessor, Store}
      end
  end.

handover(Id, Store, Nkey, Npid) ->
  {Keep, NotKeep} = storage:split(Nkey, Id, Store),
  Npid ! {handover, NotKeep},
  Keep.

stabilize(Pred, Id, Successor) ->
  {Skey, Spid} = Successor,
  case Pred of
    nil ->
      Spid ! {notify, {Id, self()}},
      Successor;
    {Id, _} ->
      Successor;
    {Skey,_} ->
      Spid ! {notify, {Id, self()}},
      Successor;
    {Xkey, Xpid} ->
      case key:between(Xkey, Id, Skey) of
        true ->
          stabilize({Xkey, Xpid}),
          {Xkey, Xpid};
        false ->
          Spid ! {notify, {Id, self()}},
          Successor
      end
  end.

create_probe(Id, Successor, Store) ->
  {_, Spid} = Successor,
  Start = now(),
  %io:format("Start: ~w ms~n", [Start]),
  %io:format("Node ~w store~w~n", [Id, Store]),
  Spid ! {probe, Id, [Id], Start}.

forward_probe(Ref, T, Nodes, Id, Successor, Store) ->
  {_, Spid} = Successor,
  %io:format("Node ~w store~w~n", [Id, Store]),
  Spid ! {probe, Ref, lists:append(Nodes, [Id]), T}.

remove_probe(Start, Nodes)->
  End =now(),
  Diff = (timer:now_diff(End, Start) div 1000),
  io:format("End: ~w ms~n", [Diff]),
  %io:format("End: ~w ms~n", [Diff]),
  %Diff = timer:now_diff(Now,Start),
  %io:format("Probe: ~w ms~n", [Diff]),
  io:format("Ring structure: ~w~n Ring length: ~w~n", [Nodes, erlang:length(Nodes)]).

add(Key, Value, Qref, Client, Id, {Pkey,_}, {_, Spid}, Store)->
  case key:between(Key, Pkey, Id) of
    true ->
      Client ! {Qref, ok},
      storage:add(Key, Value, Store);
    false ->
      Spid ! {add, Key, Value, Qref, Client},
      Store
  end.

lookup(Key, Qref, Client, Id, {Pkey,_}, {_, Spid}, Store)->
  %io:format("~w receive lookup request for ~w~n", [Id, Key]),
  case key:between(Key, Pkey, Id) of
    true->
      Result = storage:lookup(Key, Store),
      Client ! {Qref, Result};
    false ->
      Spid ! {lookup, Key, Qref, Client}
  end.