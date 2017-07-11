-module(tql_maps).

%% API

-export([ mergeWith/3
        , mergeWith3/4
        ]).

%%%---------------------------------------------------------------------
%%% API
%%%---------------------------------------------------------------------

-spec mergeWith(fun((V, V) -> V), #{K => V}, #{K => V}) -> #{K => V}.
mergeWith(Combine, M1, M2) ->
  lists:foldl(
    fun (K, M) ->
        case maps:is_key(K, M) of
          true  -> M#{K => Combine(maps:get(K, M1), maps:get(K, M2))};
          false -> M#{K => maps:get(K, M2)}
        end
    end,
    M1,
    maps:keys(M2)
   ).

-spec mergeWith3(fun((V, V) -> V), #{K => V}, #{K => V}, #{K => V})
              -> #{K => V}.
mergeWith3(Combine, M1, M2, M3) ->
  mergeWith(Combine, mergeWith(Combine, M1, M2), M3).

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 72
%% coding: latin-1
%% End:
