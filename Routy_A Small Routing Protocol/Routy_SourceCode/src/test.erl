%%%-------------------------------------------------------------------
%%% @author Wen Jian
%%% @copyright (C) 2023, KTH Royal Institute of Technology
%%% @doc
%%% Assignment HW2 for ID2201 - Distributed Systems
%%% @end
%%% Created : 15. 9月 2023 19:20
%%%-------------------------------------------------------------------
-module(test).
-author("Wen Jian").

%% API
-export([init/0, intfconf/0, broadcast/0, update/0, getstatus/1, shutdown/1, initTeyvat/0, intfconfTeyvat/0]).

init()->
  Address = 'sweden@130.229.136.49',
  routy:start({r1, Address}, stockholm),
  routy:start({r2, Address}, uppsala),
  routy:start({r3, Address}, lund),
  routy:start({r4, Address}, malmo),
  routy:start({r5, Address}, goteborg).

intfconf()->
  Address = 'sweden@130.229.136.49',

%%  % Graph A
%%  r1 ! {add, lund, {r3, Address}},
%%  r1 ! {add, malmo, {r4, Address}},
%%  r2 ! {add, lund, {r3, Address}},
%%  r2 ! {add, goteborg, {r5, Address}},
%%  r3 ! {add, uppsala, {r2, Address}},
%%  r3 ! {add, stockholm, {r1, Address}},
%%  r4 ! {add, stockholm, {r1, Address}},
%%  r4 ! {add, goteborg, {r5, Address}}.

  % Graph B
  r1 ! {add, uppsala, {r2, Address}},
  r2 ! {add, lund, {r3, Address}},
  r3 ! {add, stockholm, {r1, Address}},
  r3 ! {add, malmo, {r4, Address}},
  r4 ! {add, goteborg, {r5, Address}},
  r5 ! {add, uppsala, {r2, Address}}.

initTeyvat() ->
  % Genshin Impact - Fontaine
  Address_Teyvat = 'Fontaine@192.168.31.10',

  routy:start({r1, Address_Teyvat}, courtFontaine),
  routy:start({r2, Address_Teyvat}, marcotteStation),
  routy:start({r3, Address_Teyvat}, elynas),
  routy:start({r4, Address_Teyvat}, poisson),
  routy:start({r5, Address_Teyvat}, fountainLucine).

intfconfTeyvat() ->
  % Genshin Impact - Fontaine
  Address_Teyvat = 'Fontaine@192.168.31.10',

  r1 ! {add, marcotteStation, {r2, Address_Teyvat}},
  r2 ! {add, elynas, {r3, Address_Teyvat}},
  r3 ! {add, courtFontaine, {r1, Address_Teyvat}},
  r3 ! {add, poisson, {r4, Address_Teyvat}},
  r4 ! {add, fountainLucine, {r5, Address_Teyvat}},
  r5 ! {add, marcotteStation, {r2, Address_Teyvat}}.

broadcast() ->
  r1 ! broadcast,
  r2 ! broadcast,
  r3 ! broadcast,
  r4 ! broadcast,
  r5 ! broadcast.

update() ->
  r1 ! update,
  r2 ! update,
  r3 ! update,
  r4 ! update,
  r5 ! update.

getstatus(Reg) ->
  Reg ! {status, self()},
  receive
    {status, {Name, N, Hist, Intf, Table, Map}} ->
      io:format(" Node = ~w ~n N = ~w ~n Hist = ~w ~n Intf = ~w ~n Table = ~w ~n Map = ~w ~n", [Name, N, Hist, Intf, Table, Map]),
      ok
  end.

shutdown(Reg) ->
  routy:stop(Reg).
