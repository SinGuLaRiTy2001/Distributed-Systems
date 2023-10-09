%%%-------------------------------------------------------------------
%%% @author Wen Jian
%%% @copyright (C) 2023, KTH Royal Institute of Technology
%%% @doc
%%% Assignment HW5 for ID2201 - Distributed Systems
%%% @end
%%% Created : 07. 10月 2023 14:08
%%%-------------------------------------------------------------------
-module(node1).
-author("Wen Jian").

%% API
-compile([export_all]).

-define(Stabilize, 100).
-define(Timeout, 10000).

node(Id, Predecessor, Successor) ->
  receive
    {key, Qref, Peer} ->  % a peer needs to know our key
      Peer ! {Qref, Id},
      node(Id, Predecessor, Successor);
    {notify, New} ->  % a new node informs us of its existence
      Pred = notify(New, Id, Predecessor),
      node(Id, Pred, Successor);
    {request, Peer} ->  % a predecessor needs to know our predecessor
      request(Peer, Predecessor),
      node(Id, Predecessor, Successor);
    {status, Pred} ->  % our successor informs us about its predecessor
      Succ = stabilize(Pred, Id, Successor),
      node(Id, Predecessor, Succ);
    stabilize ->
      stabilize(Successor),
      node(Id, Predecessor, Successor);
    probe ->
      create_probe(Id, Successor),
      node(Id, Predecessor, Successor);
    {probe, Id, Nodes, T} ->
      remove_probe(T, Nodes),
      node(Id, Predecessor, Successor);
    {probe, Ref, Nodes, T} ->
      forward_probe(Ref, T, Nodes, Id, Successor),
      node(Id, Predecessor, Successor)
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
  node(Id, Predecessor, Successor).

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

schedule_stabilize() ->
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

notify({Nkey, Npid}, Id, Predecessor) ->
  case Predecessor of
    nil ->
      {Nkey, Npid};
    {Pkey, _} ->
      case key:between(Nkey, Pkey, Id) of
        true ->
          {Nkey,Npid};
        false ->
          Predecessor
      end
  end.

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

create_probe(Id, Successor) ->
  {_, Spid} = Successor,
  Start = now(),
  io:format("Start: ~w ms~n", [Start]),
  Spid ! {probe, Id, [Id], Start}.

forward_probe(Ref, T, Nodes, Id, Successor) ->
  {_, Spid} = Successor,
  Spid ! {probe, Ref, lists:append(Nodes, [Id]), T}.

remove_probe(Start, Nodes)->
  Now = now(),
  io:format("End: ~w ms~n", [Now]),
  Diff = timer:now_diff(Now, Start),
  io:format("Probe: ~w ms~n", [Diff]),
  io:format("Ring structure: ~w~nRing length: ~w~n", [Nodes, erlang:length(Nodes)]).

