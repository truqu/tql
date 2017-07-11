-module(tql_maps_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("proper/include/proper.hrl").

-export([ all/0
          %% Tests
        , mergeWith/1
        , mergeWithHasAllKeys/1
        , mergeWithCombinesValues/1
        , mergeWith3/1
        ]
       ).

all() ->
  [ mergeWithHasAllKeys
  , mergeWithCombinesValues
  , mergeWith3
  ].

%%%---------------------------------------------------------------------
%%% Tests
%%%---------------------------------------------------------------------

mergeWith(_Config) ->
  M1 = #{a => 1, b => 2},
  M2 = #{a => 3, c => 3},
  M = tql_maps:mergeWith(fun (X, Y) -> X + Y end, M1, M2),
  4 = maps:get(a, M),
  2 = maps:get(b, M),
  3 = maps:get(c, M),
  ok.

mergeWithHasAllKeys(_Config) ->
  true =
    proper:quickcheck(
      proper:forall(
        tuple([ random_map(string(), integer())
              , random_map(string(), integer())
              ]),
        fun({Map1, Map2}) ->
            Map = tql_maps:mergeWith(
                    fun(X, Y) -> X + Y end, Map1, Map2
                   ),
            Keys1 = sets:from_list(maps:keys(Map1)),
            Keys2 = sets:from_list(maps:keys(Map2)),
            Keys = sets:from_list(maps:keys(Map)),
            tql_sets:equal(sets:union(Keys1, Keys2), Keys)
        end
       )
     ),
  ok.

mergeWithCombinesValues(_Config) ->
  F = fun(X,Y) -> X + Y end,
  true =
    proper:quickcheck(
      proper:forall(
        tuple([ random_map(string(), integer())
              , random_map(string(), integer())
              ]),
        fun({M1, M2}) ->
            M = tql_maps:mergeWith(F, M1, M2),
            lists:foldl(
              fun(K, Acc) ->
                  V1 = maps:get(K, M1, 0),
                  V2 = maps:get(K, M2, 0),
                  V = maps:get(K, M),
                  (V == F(V1, V2)) andalso Acc
              end,
              true,
              maps:keys(M)
             )
        end)
     ).

mergeWith3(_Config) ->
  M1 = #{a => 1, b => 2},
  M2 = #{a => 3, c => 3},
  M3 = #{a => 1, c => 1},
  M = tql_maps:mergeWith3(fun (X, Y) -> X + Y end, M1, M2, M3),
  5 = maps:get(a, M),
  2 = maps:get(b, M),
  4 = maps:get(c, M),
  3 = maps:size(M),
  ok.

%%%---------------------------------------------------------------------
%%% Helper functions
%%%---------------------------------------------------------------------

random_map(KeyGen, ValGen) ->
  ?LET( L
      , list(tuple([KeyGen, ValGen]))
      , maps:from_list(L)
      ).

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 72
%% coding: latin-1
%% End:
