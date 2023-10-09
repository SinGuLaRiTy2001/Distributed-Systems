%%%-------------------------------------------------------------------
%%% @author Wen Jian
%%% @copyright (C) 2023, KTH Royal Institute of Technology
%%% @doc
%%% Assignment HW5 for ID2201 - Distributed Systems
%%% @end
%%% Created : 08. 10月 2023 15:31
%%%-------------------------------------------------------------------
-module(node3).
-author("Wen Jian").

%% API
-compile([export_all]).

-define(Stabilize, 100).
-define(Timeout, 10000).

node(Id, Predecessor, Successor, Store, Next) ->
  receive
    {key, Qref, Peer} ->
      Peer ! {Qref, Id},
      node(Id, Predecessor, Successor, Store, Next);
    {notify, New} ->
      {Pred, KeptStore} = notify(New, Id, Predecessor, Store),
      node(Id, Pred, Successor, KeptStore, Next);
    {request, Peer} ->
      request(Peer, Predecessor, Successor),
      node(Id, Predecessor, Successor, Store, Next);
    {status, Pred, Nx} ->
      {Succ, NewNext} = stabilize(Pred, Nx, Id, Successor),
      node(Id, Predecessor, Succ, Store, NewNext);
    stabilize ->
      stabilize(Successor),
      node(Id, Predecessor, Successor, Store, Next);
    probe->
      create_probe(Id, Successor, Store),
      node(Id, Predecessor, Successor, Store, Next);
    {probe, Id, Nodes, T} ->
      remove_probe(T, Nodes),
      node(Id, Predecessor, Successor, Store, Next);
    {probe, Ref, Nodes, T} ->
      forward_probe(Ref, T, Nodes, Id, Successor, Store),
      node(Id, Predecessor, Successor, Store, Next);
    {add, Key, Value, Qref, Client} ->
      Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Added, Next);
    {lookup, Key, Qref, Client}->
      lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Store, Next);

    {handover, Elements} ->
      Merged = storage:merge(Store, Elements),
      node(Id, Predecessor, Successor, Merged, Next);
    {'DOWN', Ref, process,_,_}->
      {Pred, Succ, Nxt} = down(Ref, Predecessor, Successor, Next),
      node(Id, Pred, Succ, Store, Nxt)
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
  node(Id, Predecessor, Successor, storage:create(), Next).

connect(Id, nil) ->
  {ok, {Id, nil, self()}};
connect(_, Peer)->
  Qref = make_ref(),
  Peer ! {key, Qref, self()},
  receive
    {Qref, Skey}->
      {ok,{Skey, monitor(Peer), Peer}}
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
    {Pkey,_,Ppid}->
      Peer ! {status, {Pkey, Ppid}, {Skey, Spid}}
  end.

notify({Nkey, Npid}, Id, Predecessor, Store)->
  case Predecessor of
    nil ->
      Keep = handover(Id, Store, Nkey, Npid),
      Newref = monitor(Npid),
      {{Nkey, Newref, Npid}, Keep};
    {Pkey,Pref, _} ->
      case key:between(Nkey, Pkey, Id) of
        true ->
          Keep = handover(Id, Store, Nkey, Npid),
          drop(Pref),
          Nref = monitor(Npid),
          {{Nkey,Nref,Npid}, Keep};
        false->
          {Predecessor, Store}
      end
  end.

handover(Id, Store, Nkey, Npid)->
  {Keep, NotKeep} = storage:split(Nkey, Id, Store),
  Npid ! {handover, NotKeep},
  Keep.

stabilize(Pred, Nx, Id, Successor)->
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
        true->
          drop(Sref),
          Newref = monitor(Xpid),
          stabilize({Xkey, Newref, Xpid}),
          {{Xkey, Newref, Xpid}, {Skey, Spid}};
        false->
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
  Spid ! {probe, Ref, lists:append(Nodes, [Id]), T}.

remove_probe(Start, Nodes)->
  End =now(),
  Diff = (timer:now_diff(End, Start) div 1000),
  io:format("End: ~w ms~n", [Diff]),
  %io:format("End: ~w ms~n", [Diff]),
  %Diff = timer:now_diff(Now,Start),
  %io:format("Probe: ~w ms~n", [Diff]),
  io:format("Ring structure: ~w~nRing length: ~w~n", [Nodes,erlang:length(Nodes)]).

add(Key, Value, Qref, Client, Id, {Pkey,_,_}, {_,_, Spid}, Store)->
  case key:between(Key, Pkey, Id) of
    true->
      Client ! {Qref, ok},
      storage:add(Key, Value, Store);
    false->
      Spid ! {add, Key, Value, Qref, Client},
      Store
  end.

lookup(Key, Qref, Client, Id, {Pkey,_,_}, {_,_, Spid}, Store)->
  case key:between(Key, Pkey, Id) of
    true->
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

down(Ref, {_, Ref, _}, Successor, Next) ->
  {nil, Successor, Next};
down(Ref, Predecessor, {_, Ref, _}, {Nkey, Npid}) ->
  Nref = monitor(Npid),
  NewSuccessor = {Nkey, Nref, Npid},
  stabilize(NewSuccessor),
  {Predecessor, NewSuccessor, nil}.
