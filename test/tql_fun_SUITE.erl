-module(tql_fun_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("proper/include/proper.hrl").

-export([ all/0
          %% Tests
        , compose/1
        , negate/1
        , sequence/1
        ]
       ).

all() ->
  [ compose
  , negate
  , sequence
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

negate(_Config) ->
  F = fun (X) -> X end,
  G = tql_fun:negate(F),
  false = G(true),
  ok.

sequence(_Config) ->
  Fs = [ fun (X) -> X + 1 end
       , fun (X) -> X + 2 end
       , fun (X) -> X + 3 end
       ],
  [1,2,3] = tql_fun:sequence(Fs, 0),
  ok.

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 72
%% coding: latin-1
%% End:
