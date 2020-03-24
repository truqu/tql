-module(tql_lists_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("proper/include/proper.hrl").

-export([ all/0
          %% Tests
        , all_/1
        , any/1
        , uniq/1
        , groups_of/1
        ]
       ).

all() ->
  [ all_
  , any
  , uniq
  , groups_of
  ].

%%%---------------------------------------------------------------------
%%% Tests
%%%---------------------------------------------------------------------

all_(_Config) ->
  true = proper:quickcheck(
           ?FORALL( L
                  , list(boolean())
                  , ?IMPLIES(not lists:member(false, L)
                            , tql_lists:all(L)
                            )
                  )
          ),
  ok.

any(_Config) ->
  true = proper:quickcheck(
           ?FORALL( L
                  , list(boolean())
                  , ?IMPLIES(lists:member(true, L)
                            , tql_lists:any(L)
                            )
                  )
          ),
  ok.

uniq(_Config) ->
  true = proper:quickcheck(
           ?FORALL( L
                  , list(term())
                  , length(tql_lists:uniq(L)) == sets:size(sets:from_list(L))
                  )
          ),
  true = proper:quickcheck(
           ?FORALL( L
                  , list(term())
                  , tql_lists:all(
                      [lists:member(X, tql_lists:uniq(L)) || X <- L]
                     )
                  )
          ),
  ok.

groups_of(_Config) ->
  true = proper:quickcheck(
           ?FORALL( {List, Size}
                  , {list(any()), pos_integer()}
                  , proper:conjunction(
                      [ {append_is_original, check_append_is_original(List, Size)}
                      , {groups_are_sized, check_groups_are_sized(List, Size)}
                      ])
                  )
          ),
  ok.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

check_append_is_original(List, Size) ->
  List == lists:append(tql_lists:groups_of(Size, List)).

check_groups_are_sized(List, Size) ->
  check_group_sizes(tql_lists:groups_of(Size, List), Size).

check_group_sizes([], _) -> true;
check_group_sizes([Xs], Size) -> Xs /= [] andalso length(Xs) =< Size;
check_group_sizes([Xs | Rest], Size) -> length(Xs) == Size andalso check_group_sizes(Rest, Size).

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 72
%% coding: latin-1
%% End:
