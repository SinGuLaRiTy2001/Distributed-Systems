%%%-------------------------------------------------------------------
%%% @author Wen Jian
%%% @copyright (C) 2023, KTH Royal Institute of Technology
%%% @doc
%%% Assignment HW1 for ID2201 - Distributed Systems
%%% @end
%%% Created : 02. 9月 2023 19:03
%%%-------------------------------------------------------------------
-module(main).
-author("Wen Jian").

%% API
-export([start/2, stop/0]).

start(Port, N) ->
  register(rudy, spawn(fun() -> rudy:init(Port, N) end)).  % Create a process to run the server and register the process in the system.

stop() ->
  %exit(whereis(rudy), "terminated").
  rudy ! stop.  % Send stop message to rudy module to terminate Listen.
