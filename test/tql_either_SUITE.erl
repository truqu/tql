-module(tql_either_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("proper/include/proper.hrl").

-export([ all/0
          %% Tests
        , test_either1/1
        , test_either2/1
        , test_either3/1
        , test_either4/1
        ]
       ).

all() ->
  [ test_either1
  , test_either2
  , test_either3
  , test_either4
  ].

%%%---------------------------------------------------------------------
%%% Tests
%%%---------------------------------------------------------------------

test_either1(_Config) ->
  Result = tql_either:fold([ fun initiate_map/0
                           , fun(X) -> add_foo(X, 1) end
                           , fun increment_foo/1
                           , fun maps:to_list/1
                           ]),
  {ok, [{foo, 2}]} = Result.

test_either2(_Config) ->
  Result = tql_either:fold( #{}
                          , [ fun(X) -> add_foo(X, 1) end
                            , fun increment_foo/1
                            , fun maps:to_list/1
                            ]),
  {ok, [{foo, 2}]} = Result.


test_either3(_Config) ->
  Result = tql_either:fold( #{foo => 2}
                          , [ fun(X) -> add_foo(X, 1) end
                            , fun increment_foo/1
                            , fun maps:to_list/1
                            ]),
  {error, foo_already_set} = Result.

test_either4(_Config) ->
  Result = tql_either:fold( #{}
                          , [ fun(X) -> add_foo(X, "BAR") end
                            , fun increment_foo/1
                            , fun maps:to_list/1
                            ]),
  {error, foo_not_integer} = Result.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------


initiate_map() ->
  #{}.

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
