-module(tql_compare_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("proper/include/proper.hrl").

-export([ all/0 ]).

-export([ properties/1
        , by/1
        , by_prop/1
        , concat/1
        ]).

all() ->
    [ properties
    , by
    , by_prop
    , concat
    ].

%%%---------------------------------------------------------------------
%%% Tests
%%%---------------------------------------------------------------------

properties(_Config) ->
  true =
    proper:quickcheck(
      proper:forall( list(term())
                   , fun prop_by_id_is_noop/1
                   )
     ),
  true =
    proper:quickcheck(
      proper:forall( list(term())
                   , fun prop_reverse_reverse_is_id/1
                   )
     ),
  true = 
    proper:quickcheck(
     proper:forall( list(term())
                  , fun prop_desc_is_reverse/1
                  )
     ),
  ok.

by(_Config) ->
  F = fun ({_, V}) -> V end,
  Xs = [{foo, 20}, {bar, 40}, {baz, 10}, {etc, 30}],
  Ys = [{baz, 10}, {foo, 20}, {etc, 30}, {bar, 40}],
  Ry = lists:reverse(Ys),
  Ys = lists:sort(tql_compare:by(F, ascending), Xs),
  Ry = lists:sort(tql_compare:by(F, descending), Xs),
  ok.

by_prop(_Config) ->
  Xs = [ #{a => 0, b => 10}
       , #{a => 1, b => 9}
       , #{a => 2, b => 8}
       , #{a => 3, b => 7}
       ],
  Ys = lists:reverse(Xs),
  Xs = lists:sort(tql_compare:by_prop(a), Xs),
  Ys = lists:sort(tql_compare:by_prop(b), Xs),
  ok.

concat(_Config) ->
  Xs = [ #{a => 0, b => 0}
       , #{a => 1, b => 2}
       , #{a => 3}
       , #{a => 1, b => 1}
       , #{a => 2}
       , #{a => 0, b => 3}
       , #{a => 3, b => 5}
       ],
  Ys = [ #{a => 0, b => 3}
       , #{a => 0, b => 0}
       , #{a => 1, b => 2}
       , #{a => 1, b => 1}
       , #{a => 2}
       , #{a => 3, b => 5}
       , #{a => 3}
       ],
  ByY = tql_compare:by_prop(b, 0, descending),
  ByX = tql_compare:by_prop(a),
  Comp = tql_compare:concat([ByX, ByY]),
  Ys = lists:sort(Comp, Xs),
  ok.

%%%---------------------------------------------------------------------
%%% Properties
%%%---------------------------------------------------------------------

prop_desc_is_reverse(Xs) ->
  Comp = tql_compare:by(fun tql:id/1),
  Rev = tql_compare:reverse(Comp),
  Desc = tql_compare:by(fun tql:id/1, descending),
  lists:sort(Desc, Xs) =:= lists:sort(Rev, Xs).

prop_by_id_is_noop(Xs) ->
  lists:sort(Xs) =:= lists:sort(tql_compare:by(fun tql:id/1), Xs).

prop_reverse_reverse_is_id(Xs) ->
  Comp = tql_compare:by(fun tql:id/1),
  Rev1 = tql_compare:reverse(Comp),
  Rev2 = tql_compare:reverse(Rev1),
  lists:sort(Xs) =:= lists:sort(Rev2, Xs).

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 72
%% coding: latin-1
%% End:
