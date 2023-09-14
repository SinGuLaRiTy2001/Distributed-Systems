%%%-------------------------------------------------------------------
%%% @author Wen Jian
%%% @copyright (C) 2023, KTH Royal Institute of Technology
%%% @doc
%%% Assignment HW1 for ID2201 - Distributed Systems
%%% @end
%%% Created : 02. 9月 2023 18:29
%%%-------------------------------------------------------------------
-module(rudy).
-author("Wen Jian").

%% API
-export([init/2]).

init(Port, N) ->
  Opt = [list, {active, false}, {reuseaddr, true}],
  case gen_tcp:listen(Port, Opt) of
    {ok, Listen} ->  % Open a listening socket.
      handler_pool(Listen, N),  % Create a pool of N handlers to deal with request.
      receive  % Wait for stop() command from input
        stop -> ok
      end,
      gen_tcp:close(Listen),
      ok;
    {error, _} ->
      error
  end.

handler_pool(Listen, N) ->  % Use recursion as for()-like to create N handlers.
  if
    N == 0 ->
      ok;
    true ->
      spawn(fun() -> handler(Listen) end),  % Create a new process and run a new handler
      handler_pool(Listen, N-1)  % Recursion
  end.

handler(Listen) ->
  case gen_tcp:accept(Listen) of  % Listen to the socket for an incoming connection.
    {ok, Client} ->
      request(Client),  % Receive and pass request to handle.
      handler(Listen);
    {error, _} ->
      error
  end.

request(Client) ->
  Recv = gen_tcp:recv(Client, 0),  % Read request from client.
  case Recv of
    {ok, Str} ->
      Request = http:parse_request(Str),  % Parse request through http module.
      Response = reply(Request),  % Generate reply.
      gen_tcp:send(Client, Response);  % Send response back to client.
    {error, Error} ->
      io:format("rudy: error: ~w~n", [Error])
  end,
  gen_tcp:close(Client).

reply({{get, _, _}, _, Body}) ->
  timer:sleep(40),
  http:ok(Body).
