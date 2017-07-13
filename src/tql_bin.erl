-module(tql_bin).

%% API

-export([ to_hex/1
        ]
       ).

%%%---------------------------------------------------------------------
%%% API
%%%---------------------------------------------------------------------

to_hex(Bin) when is_binary(Bin) ->
  list_to_binary([io_lib:format("~2.16.0b", [B]) || <<B>> <= Bin]).

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 72
%% coding: latin-1
%% End:
