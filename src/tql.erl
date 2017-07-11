-module(tql).

%% API exports
-export([ binary_join/2
        , id/1
        , to_hex/1
        ]).

%%%---------------------------------------------------------------------
%%% API
%%%---------------------------------------------------------------------

binary_join([], _Sep) ->
  <<>>;
binary_join([Part], _Sep) ->
  Part;
binary_join(List, Sep) ->
  lists:foldr(
    fun (A, B) ->
        case bit_size(B) > 0 of
          true  -> <<A/binary, Sep/binary, B/binary>>;
          false -> A
        end
    end,
    <<>>,
    List
   ).

-spec id(A) -> A.
id(X) ->
  X.

to_hex(Bin) when is_binary(Bin) ->
  list_to_binary(
    lists:flatten(
      [io_lib:format("~2.16.0b", [B]) || <<B>> <= Bin])).

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 72
%% coding: latin-1
%% End:
