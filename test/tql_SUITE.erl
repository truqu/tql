-module(tql_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("proper/include/proper.hrl").

-export([ all/0
          %% Tests
        , id/1
        , pipe/1
        ]
       ).

all() ->
  [ id
  , pipe
  ].

%%%---------------------------------------------------------------------
%%% Tests
%%%---------------------------------------------------------------------

id(_Config) ->
  true = proper:quickcheck(
           proper:forall(term(), fun (X) -> tql:id(X) =:= X end)
          ),
  ok.

pipe(_Config) ->
  3 = tql:pipe(1, [fun (X) -> X * 2 end, fun (X) -> X +1 end]).

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 72
%% coding: latin-1
%% End:
