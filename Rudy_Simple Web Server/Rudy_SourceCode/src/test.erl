%%%-------------------------------------------------------------------
%%% @author Wen Jian
%%% @copyright (C) 2023, KTH Royal Institute of Technology
%%% @doc
%%% Assignment HW1 for ID2201 - Distributed Systems
%%% @end
%%% Created : 02. 9月 2023 19:10
%%%-------------------------------------------------------------------
-module(test).
-author("Wen Jian").

%% API
-export([bench/4]).

bench(Host, Port, N, ThreadNum) ->
  Start = erlang:system_time(micro_seconds),
  create_thread(Host, Port, N, ThreadNum, self()),  % Create ThreadNum of processes (as client), each process contains N requests.
  wait_thread(ThreadNum),  % Wait until all the threads complete.
  Finish = erlang:system_time(micro_seconds),
  io:format("Benchmark complete, time: ~w us. ~n", [Finish - Start]).

create_thread(Host, Port, N, ThreadNum, Done) ->  % Use recursion to create ThreadNums of processes (as client).
  if
    ThreadNum == 0 ->
      ok;
    true ->
      spawn(fun() -> run(N, Host, Port, Done) end),  % Spawn a process.
      create_thread(Host, Port, N, ThreadNum - 1, Done)
  end.

wait_thread(ThreadNum) ->  % Wait until all the threads complete.
  if
    ThreadNum == 0 ->
      ok;
    true ->
      receive
        ok ->
          wait_thread(ThreadNum - 1)
      end
  end.

run(N, Host, Port, Done) ->  % For each process, send N requests through recursion.
  if
    N == 0 ->
      Done ! ok;
    true ->
      request(Host, Port),
      run(N-1, Host, Port, Done)
  end.

request(Host, Port) ->
  Opt = [list, {active, false}, {reuseaddr, true}],
  {ok, Server} = gen_tcp:connect(Host, Port, Opt),
  gen_tcp:send(Server, http:get("jianwen")),
  Recv = gen_tcp:recv(Server, 0),
  case Recv of
    {ok, _} ->
      ok;
    {error, Error} ->
      io:format("test: error: ~w~n", [Error])
  end,
  gen_tcp:close(Server).
