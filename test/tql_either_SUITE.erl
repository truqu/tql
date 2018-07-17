-module(tql_either_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("proper/include/proper.hrl").

-export([ all/0
          %% Tests
        , test_fold1/1
        , test_fold2/1
        , test_fold3/1
        , test_sequence1/1
        , test_sequence2/1
        , test_sequence3/1
        , test_from_bool/1
        ]).

all() ->
  [ test_fold1
  , test_fold2
  , test_fold3
  , test_sequence1
  , test_sequence2
  , test_sequence3
  , test_from_bool
  ].

%%%---------------------------------------------------------------------
%%% Tests
%%%---------------------------------------------------------------------

test_fold1(_Config) ->
  Result = tql_either:fold( #{}
                          , [ fun(X) -> add_foo(X, 1) end
                            , fun increment_foo/1
                            , fun maps:to_list/1
                            ]),
  {ok, [{foo, 2}]} = Result.


test_fold2(_Config) ->
  Result = tql_either:fold( #{foo => 2}
                          , [ fun(X) -> add_foo(X, 1) end
                            , fun increment_foo/1
                            , fun maps:to_list/1
                            ]),
  {error, foo_already_set} = Result.

test_fold3(_Config) ->
  Result = tql_either:fold( #{}
                          , [ fun(X) -> add_foo(X, "BAR") end
                            , fun increment_foo/1
                            , fun maps:to_list/1
                            ]),
  {error, foo_not_integer} = Result.


test_sequence1(_Config) ->
  Result = tql_either:sequence([ {ok, true}
                               , {ok, 1}
                               , {ok, "foobar"}
                               ]),
  {ok, [true, 1, "foobar"]} = Result.

test_sequence2(_Config) ->
  Result = tql_either:sequence([ {ok, true}
                               , {error, not_found}
                               , {ok, "foobar"}
                               ]),
  {error, not_found} = Result.


test_sequence3(_Config) ->
  Result = tql_either:sequence([ {ok, true}
                               , {error, not_found}
                               , {error, ignored}
                               ]),
  {error, not_found} = Result.

test_from_bool(_Config) ->
  Result1 = tql_either:from_bool(authorized, unauthorized, true),
  Result2 = tql_either:from_bool(authorized, unauthorized, false),
  {ok, authorized} = Result1,
  {error, unauthorized} = Result2.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------


add_foo(#{ foo := _ }, _Value) ->
  {error, foo_already_set};
add_foo(Map, Value) ->
  {ok, maps:put(foo, Value, Map)}.

increment_foo(#{ foo := Foo } = Map) when is_integer(Foo) ->
  {ok, maps:update(foo, Foo + 1, Map)};
increment_foo(_Map) ->
  {error, foo_not_integer}.

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 72
%% coding: latin-1
%% End:
