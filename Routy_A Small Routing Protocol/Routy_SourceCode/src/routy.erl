%%%-------------------------------------------------------------------
%%% @author Wen Jian
%%% @copyright (C) 2023, KTH Royal Institute of Technology
%%% @doc
%%% Assignment HW2 for ID2201 - Distributed Systems
%%% @end
%%% Created : 15. 9月 2023 18:51
%%%-------------------------------------------------------------------
-module(routy).
-author("Wen Jian").

%% API
-export([start/2, stop/1]).

%%start(Reg, Name) ->
%%  register(Reg, spawn(fun() -> init(Name) end)).

start(Pid, Name) ->  % Modified start and init to customize Pid
  % {r1, 'sweden@192.168.50.59'}, stockholm
  % <process id> Pid = {r1, 'sweden@192.168.50.59'}, <router register> Reg = r1, <symbolic name> Name = stockholm
  {Reg, _} = Pid,
  register(Reg, spawn(fun() -> init(Name, Pid) end)).  % For each router, launch a process

stop(Node) ->
  Node ! stop,
  unregister(Node).

%%init(Name) ->
%%  Intf = intf:new(),
%%  Map = map:new(),
%%  Table = dijkstra:table(Intf, Map),
%%  Hist = hist:new(Name),
%%  router(Name, 0, Hist, Intf, Table, Map).

init(Name, Pid) ->
  Intf = intf:new(),
  Ref = erlang:monitor(process, Pid),
  NewIntf = intf:add(Name, Ref, Pid, Intf),  % Initialbroadize with itself added as interface
  Map = map:new(),
  Table = dijkstra:table(NewIntf, Map),
  Hist = hist:new(Name),
  router(Name, 0, Hist, NewIntf, Table, Map).

router(Name, N, Hist, Intf, Table, Map) ->
  receive
    {add, Node, Pid} ->
      Ref = erlang:monitor(process, Pid),
      Intf1 = intf:add(Node, Ref, Pid, Intf),
      router(Name, N, Hist, Intf1, Table, Map);

    {remove, Node} ->
      {ok, Ref} = intf:ref(Node, Intf),
      erlang:demonitor(Ref),
      Intf1 = intf:remove(Node, Intf),
      router(Name, N, Hist, Intf1, Table, Map);

    {'DOWN', Ref, process, _, _} ->
      {ok, Down} = intf:name(Ref, Intf),
      io:format("~w: exit recived from ~w~n", [Name, Down]),
      Intf1 = intf:remove(Down, Intf),
      router(Name, N, Hist, Intf1, Table, Map);

    {status, From} ->
      From ! {status, {Name, N, Hist, Intf, Table, Map}},
      router(Name, N, Hist, Intf, Table, Map);

    {links, Node, R, Links} ->  % When receiving update from others
      case hist:update(Node, R, Hist) of  % Check if update is new
        {new, Hist1} ->  % If new, 1) broadcast to adjacent reachable nodes, 2) update map
          intf:broadcast({links, Node, R, Links}, Intf),
          Map1 = map:update(Node, Links, Map),
          router(Name, N, Hist1, Intf, Table, Map1);
        old ->  % If old, ignore
          router(Name, N, Hist, Intf, Table, Map)
      end;

    {route, Name, From, Message} ->  % Message arrive :)
      io:format("~w: received message ~w from ~w ~n", [Name, Message, From]),
      router(Name, N, Hist, Intf, Table, Map);

    {route, To, From, Message} ->  % Message forward
      io:format("~w: routing message (~w) from ~w ~n", [Name, Message, From]),
      case dijkstra:route(To, Table) of  % Search for next node
        {ok, Gw} ->  % Next node found (get Name)
          case intf:lookup(Gw, Intf) of  % Search for corresponding Pid
            {ok, Pid} ->
              Pid ! {route, To, From, Message},  % Forward to next node
              io:format("~w: sent to ~w ~n", [Name, Pid]);
            notfound ->
              io:format("~w: routing failed! ~n", [Name]),
              ok
          end;
        notfound ->
          io:format("~w: routing failed! ~n", [Name]),
          ok
      end,
      router(Name, N, Hist, Intf, Table, Map);

    {send, To, Message} ->
      self() ! {route, To, Name, Message},
      router(Name, N, Hist, Intf, Table, Map);

    update ->  % Update the table by creating a new one with current Intf and Map
      Table1 = dijkstra:table(intf:list(Intf), Map),
      router(Name, N, Hist, Intf, Table1, Map);

    broadcast ->  % Prepare a message and send to adjacent reachable nodes
      Message = {links, Name, N, intf:list(Intf)},
      intf:broadcast(Message, Intf),
      router(Name, N+1, Hist, Intf, Table, Map);

    stop ->
      io:format("~w is terminated. ~n",[Name]),
      ok
  end.