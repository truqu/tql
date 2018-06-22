-module(tql_maps).

%% API

-export([ map_values/2
        , merge_with/3
        , merge_with3/4
        , update_key/3
        ]).

%%%---------------------------------------------------------------------
%%% API
%%%---------------------------------------------------------------------

%% @doc Map a function over the values of a map.
-spec map_values(fun((V) -> V), #{K => V}) -> #{K => V}.
map_values(F, M) ->
  maps:map(fun (_, V) -> F(V) end, M).

%% @doc Merge 2 maps, using the specificied `Combine' function for identical keys.
%%
%% When a key exists in both maps, `Combine' is called with the values
%% for the first and second map, using the result in the resulting map.
-spec merge_with(Combine, Map, Map) -> Map when
    Combine :: fun((V, V) -> V),
    Map :: #{K :: term() => V}.
merge_with(Combine, M1, M2) ->
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

%% @doc Merge 3 maps, using the specified `Combine' function for clashing keys.
%%
%% If a key exists in all three maps, `Combine' will first be called
%% with the values from the first and second map, then with the result of
%% that call and the value from the third map.
-spec merge_with3(Combine, Map, Map, Map) -> Map when
    Combine :: fun((V, V) -> V),
    Map :: #{K :: term() => V}.
merge_with3(Combine, M1, M2, M3) ->
  merge_with(Combine, merge_with(Combine, M1, M2), M3).

%% @doc Replace a key in a map with another key.
%%
%% If the new key already exists, its value will be replaced with the
%% value that used to be asociated with `OldKey'.
-spec update_key(OldKey, NewKey, Map) -> UpdatedMap when
    OldKey :: term(),
    NewKey :: term(),
    Map :: #{OldKey := Value :: term()},
    UpdatedMap :: #{NewKey := Value :: term()}.
update_key(Old, New, M) ->
  V = maps:get(Old, M),
  M2 = maps:remove(Old, M),
  M2#{New => V}.

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 72
%% coding: latin-1
%% End:
