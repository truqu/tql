-module(tql_bin_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("proper/include/proper.hrl").

-export([ all/0
          %% Tests
        , to_hex/1
        ]
       ).

all() ->
  [ to_hex
  ].

%%%---------------------------------------------------------------------
%%% Tests
%%%---------------------------------------------------------------------

to_hex(_Config) ->
  <<"beef">> = tql_bin:to_hex(<<190, 239>>),
  <<"c0ffee">> = tql_bin:to_hex(<<192, 255, 238>>),
  ok.

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 72
%% coding: latin-1
%% End:
