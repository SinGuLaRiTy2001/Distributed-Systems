%%%-------------------------------------------------------------------
%%% @author Wen Jian
%%% @copyright (C) 2023, KTH Royal Institute of Technology
%%% @doc
%%% Assignment HW1 for ID2201 - Distributed Systems
%%% @end
%%% Created : 02. 9月 2023 15:51
%%%-------------------------------------------------------------------
-module(http).
-author("Wen Jian").

%% API
-export([parse_request/1, ok/1, get/1]).

%%  Request = Request-Line ;                  Section 5.1
%%            *(( general-header ;            Section 4.5
%%              | request-header ;            Section 5.3
%%              | entity-header ) CRLF) ;     Section 7.1
%%            CRLF
%%            [ message-body ] ;              Section 4.3

parse_request(R0) ->
  {Request, R1} = request_line(R0),
  {Headers, R2} = headers(R1),
  {Body, _} = message_body(R2),
  {Request, Headers, Body}.

request_line([$G, $E, $T, 32 |R0]) ->
  {URI, R1} = request_uri(R0),
  {Ver, R2} = http_version(R1),
  [13,10|R3] = R2,
  {{get, URI, Ver}, R3}.

request_uri([32|R0])->
  {[], R0};
request_uri([C|R0]) ->
  {Rest, R1} = request_uri(R0),
  {[C|Rest], R1}.

http_version([$H, $T, $T, $P, $/, $1, $., $1 | R0]) ->
  {v11, R0};
http_version([$H, $T, $T, $P, $/, $1, $., $0 | R0]) ->
  {v10, R0}.

headers([13,10|R0]) ->
  {[],R0};
headers(R0) ->
  {Header, R1} = header(R0),
  {Rest, R2} = headers(R1),
  {[Header|Rest], R2}.
header([13,10|R0]) ->
  {[], R0};
header([C|R0]) ->
  {Rest, R1} = header(R0),
  {[C|Rest], R1}.

message_body(R) ->
  {R, []}.

ok(Body) ->
  "HTTP/1.1 200 OK\r\n" ++ "\r\n" ++ Body.

get(URI) ->
  "GET " ++ URI ++ " HTTP/1.1\r\n" ++ "\r\n".