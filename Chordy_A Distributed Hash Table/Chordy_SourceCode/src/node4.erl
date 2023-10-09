%%%-------------------------------------------------------------------
%%% @author Wen Jian
%%% @copyright (C) 2023, KTH Royal Institute of Technology
%%% @doc
%%% Assignment HW5 for ID2201 - Distributed Systems
%%% @end
%%% Created : 08. 10月 2023 16:50
%%%-------------------------------------------------------------------
-module(node4).
-author("Wen Jian").

%% API
-compile([export_all]).

-define(Stabilize, 100).
-define(Timeout, 10000).

node(Id, Predecessor, Successor, Store, Next, Replica) ->
  receive
    {key, Qref, Peer} ->
      Peer ! {Qref, Id},
      node(Id, Predecessor, Successor, Store, Next, Replica);
    {notify, New} ->
      {Pred, {KeptStore, KeptReplic}} = notify(New, Id, Predecessor, Store, Replica),
      node(Id, Pred, Successor, KeptStore, Next, KeptReplic);
    {request, Peer} ->
      request(Peer, Predecessor, Successor),
      node(Id, Predecessor, Successor, Store, Next, Replica);
    {status, Pred, Nx} ->
      {Succ, NewNext} = stabilize(Pred, Nx, Id, Successor),
      node(Id, Predecessor, Succ, Store, NewNext, Replica);
    stabilize ->
      stabilize(Successor),
      node(Id, Predecessor, Successor, Store, Next, Replica);
    probe->
      create_probe(Id, Successor, Store),
      node(Id, Predecessor, Successor, Store, Next, Replica);
    {probe, Id, Nodes, T} ->
      remove_probe(T, Nodes),
      node(Id, Predecessor, Successor, Store, Next, Replica);
    {probe, Ref, Nodes, T} ->
      forward_probe(Ref, T, Nodes, Id, Successor, Store),
      node(Id, Predecessor, Successor, Store, Next, Replica);
    {add, Key, Value, Qref, Client} ->
      Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Added, Next, Replica);
    {lookup, Key, Qref, Client} ->
      lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Store, Next, Replica);
    {handover, {ElementsS, ElementsR}} ->
      MergedS = storage:merge(Store, ElementsS),
      MergedR = storage:merge(Store, ElementsR),
      node(Id, Predecessor, Successor, MergedS, Next, MergedR);
    {'DOWN', Ref, process,_,_} ->
      io:format("Down message detected at: ~w", [Ref]),
      {Pred, Succ, Nxt, NewStore, NewReplica} = down(Ref, Predecessor, Successor, Next, Store, Replica),
      node(Id, Pred, Succ, NewStore, Nxt, NewReplica);

    {replicate, Key, Value, Qref, Client} ->
      NewReplica = add_replica(Key, Value, Qref, Client, Id, Replica),
      node(Id, Predecessor, Successor, Store, Next, NewReplica);
    status ->
      io:format(" Predecessor: ~w~n
                       Successor: ~w~n
                       Next: ~w~n
                       Store: ~w~n
                       Replica: ~w~n",[Predecessor, Successor, Next, Store, Replica]),
      node(Id, Predecessor, Successor, Store, Next, Replica)
  end.

start(Id) ->
  start(Id, nil).
start(Id, Peer) ->
  timer:start(),
  spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
  Predecessor = nil,
  Next = nil,
  {ok, Successor} = connect(Id, Peer),
  schedule_stabilize(),
  node(Id, Predecessor, Successor, storage:create(), Next, storage:create()).

connect(Id, nil) ->
  {ok, {Id, nil, self()}};
connect(_, Peer)->
  Qref = make_ref(),
  Peer ! {key, Qref, self()},
  receive
    {Qref, Skey} ->
      {ok, {Skey, monitor(Peer), Peer}}
  after ?Timeout ->
    io:format("Time out.~n",[])
  end.

schedule_stabilize()->
  timer:send_interval(?Stabilize, self(), stabilize).

stabilize({_,_,Spid}) ->
  Spid ! {request, self()}.

request(Peer, Predecessor, Successor)->
  {Skey,_,Spid} = Successor,
  case Predecessor of
    nil->
      Peer ! {status, nil, {Skey, Spid}};
    {Pkey, _, Ppid}->
      Peer ! {status, {Pkey, Ppid}, {Skey, Spid}}
  end.

notify({Nkey, Npid}, Id, Predecessor, Store, Replicate)->
  case Predecessor of
    nil ->
      Keep = handover(Id, Store, Replicate, Nkey, Npid),
      Newref = monitor(Npid),
      {{Nkey, Newref, Npid}, Keep};
    {Pkey,Pref, _} ->
      case key:between(Nkey, Pkey, Id) of
        true ->
          Keep = handover(Id, Store, Replicate, Nkey, Npid),
          drop(Pref),
          Nref = monitor(Npid),
          {{Nkey, Nref, Npid}, Keep};
        false ->
          {Predecessor, {Store, Replicate}}
      end
  end.

handover(Id, Store, Replicate, Nkey, Npid) ->
  {KeepS, NotKeepS} = storage:split(Nkey, Id, Store),
  {KeepR, NotKeepR} = storage:split(Nkey, Id, Replicate),
  Npid ! {handover, {NotKeepS,NotKeepR}},
  {KeepS, KeepR}.

stabilize(Pred, Nx, Id, Successor) ->
  {Skey, Sref, Spid} = Successor,
  case Pred of
    nil->
      Spid ! {notify, {Id, self()}},
      {Successor, Nx};
    {Id, _}->
      {Successor, Nx};
    {Skey,_}->
      Spid ! {notify, {Id, self()}},
      {Successor, Nx};
    {Xkey, Xpid} ->
      case key:between(Xkey, Id, Skey) of
        true ->
          drop(Sref),
          Newref = monitor(Xpid),
          stabilize({Xkey, Newref, Xpid}),
          {{Xkey, Newref, Xpid}, {Skey, Spid}};
        false ->
          %%notify
          Spid ! {notify, {Id, self()}},
          {Successor, Nx}
      end
  end.

create_probe(Id, Successor, Store) ->
  {_,_,Spid} = Successor,
  Start = now(),
  %io:format("Start: ~w ms~n", [Start]),
  %io:format("Node ~w store~w~n", [Id, Store]),
  Spid ! {probe, Id, [Id], Start}.

forward_probe(Ref, T, Nodes, Id, Successor, Store) ->
  {_,_,Spid} = Successor,
  %io:format("Node ~w store~w~n", [Id, Store]),
  Spid ! {probe, Ref, lists:append(Nodes, [Id]), T}.

remove_probe(Start, Nodes)->
  End =now(),
  Diff = (timer:now_diff(End, Start) div 1000),
  io:format("End: ~w ms~n", [Diff]),
  %io:format("End: ~w ms~n", [Diff]),
  %Diff = timer:now_diff(Now,Start),
  %io:format("Probe: ~w ms~n", [Diff]),
  io:format("Ring structure: ~w~n Ring length ~w~n", [Nodes,erlang:length(Nodes)]).


add(Key, Value, Qref, Client, Id, {Pkey,_,_}, {_,_,Spid}, Store)->
  case key:between(Key, Pkey, Id) of
    true ->
      Client ! {Qref, Id},
      Added = storage:add(Key, Value, Store),
      Spid ! {replicate, Key, Value, Qref, Client},
      Added;
    false ->
      Spid ! {add, Key, Value, Qref, Client},
      Store
  end.

add_replica(Key, Value, Qref, Client, Id, Replica)->
  Added = storage:add(Key, Value, Replica),
  Client ! {Qref, Id},
  Added.

lookup(Key, Qref, Client, Id, {Pkey,_,_}, {_, _,Spid}, Store)->
  %io:format("~w receive lookup request for ~w~n", [Id,Key]),
  case key:between(Key, Pkey, Id) of
    true ->
      Result = storage:lookup(Key, Store),
      Client ! {Qref, Result};
    false ->
      Spid ! {lookup, Key, Qref, Client}
  end.

monitor(Pid) ->
  erlang:monitor(process, Pid).
drop(nil) ->
  ok;
drop(Pid)->
  erlang:demonitor(Pid, [flush]).

down(Ref, {_, Ref, _}, Successor, Next, Store, Replica) ->
  {nil, Successor, Next, Store, Replica};
down(Ref, Predecessor, {_, Ref, _}, {Nkey, Npid}, Store, Replica) ->
  NewStore = recoverStore(Store, Replica),
  Nref = monitor(Npid),
  NewSuccessor = {Nkey,Nref,Npid},
  stabilize(NewSuccessor),
  {Predecessor, NewSuccessor, nil, NewStore, storage:create()}.

recoverStore(Store, Replica)->
  storage:merge(Replica, Store).

