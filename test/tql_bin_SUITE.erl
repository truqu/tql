-module(tql_bin_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("proper/include/proper.hrl").

-export([ all/0
          %% Tests
        , to_hex/1
        , trim/1
        , reverse/1
        ]).

all() ->
  [ to_hex
  , trim
  , reverse
  ].

%%%---------------------------------------------------------------------
%%% Tests
%%%---------------------------------------------------------------------

to_hex(_Config) ->
  <<"beef">> = tql_bin:to_hex(<<190, 239>>),
  <<"c0ffee">> = tql_bin:to_hex(<<192, 255, 238>>),
  ok.

trim(_Config) ->
  Binary = <<"   testing   ">>,
  <<"testing">>    = tql_bin:trim(Binary),
  <<"testing   ">> = tql_bin:trim_left(Binary),
  <<"   testing">> = tql_bin:trim_right(Binary),
  ok.

reverse(_Config) ->
  Binary = <<"testing">>,
  <<"gnitset">>    = tql_bin:reverse(Binary),
  ok.

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 72
%% coding: latin-1
%% End:
