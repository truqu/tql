-module(tql_fun_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("proper/include/proper.hrl").

-export([ all/0
          %% Tests
        , compose/1
        ]
       ).

all() ->
  [ compose
  ].

%%%---------------------------------------------------------------------
%%% Tests
%%%---------------------------------------------------------------------

compose(_Config) ->
  F = fun (X) -> X * 2 end,
  G = fun (X) -> X + 1 end,
  42 = (tql_fun:compose(F, G))(20),
  H = fun (X) -> X + 2 end,
  44 = (tql_fun:compose([H, F, G]))(20),
  ok.

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 72
%% coding: latin-1
%% End:
