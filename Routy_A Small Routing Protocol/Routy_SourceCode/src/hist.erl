%%%-------------------------------------------------------------------
%%% @author Wen Jian
%%% @copyright (C) 2023, KTH Royal Institute of Technology
%%% @doc
%%% Assignment HW2 for ID2201 - Distributed Systems
%%% @end
%%% Created : 15. 9月 2023 18:42
%%%-------------------------------------------------------------------
-module(hist).
-author("Wen Jian").

%% API
-export([new/1, update/3]).

new(Name) ->
  [{Name, 0}].

update(Node, N, History) ->  % Used to identify whether a received update is new or already known.
  case lists:keyfind(Node, 1, History) of
    {Node, OldNum} ->
      if
        N > OldNum ->  % If new (but known before), update History.
          Updated = lists:keyreplace(Node, 1, History, {Node, N}),
          {new, Updated};
        true ->  % Otherwise, return old.
          old
      end;
    false ->  % If new (never known before), update History.
      {new, [{Node, 0} | History]}
  end.