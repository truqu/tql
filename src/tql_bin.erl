-module(tql_bin).

%% API

-export([ to_hex/1
        , join/2
        ]
       ).

%%%---------------------------------------------------------------------
%%% API
%%%---------------------------------------------------------------------


%% @doc Encodes a binary in hexadecimal.
%%
%% ```
%%    R = tql_bin:to_hex(<<"foobar">>),
%%    R = <<"666f6f626172">>.
%% '''
-spec to_hex(binary()) -> binary().
to_hex(Bin) when is_binary(Bin) ->
  list_to_binary([io_lib:format("~2.16.0b", [B]) || <<B>> <= Bin]).

%% @doc Joins a list of binaries with a separator.
%%
%% ```
%%    V = tql_bin:join([<<"a">>, <<"b">>, <<"c">>], <<", ">>),
%%    V = <<"a, b, c">>.
%% '''
-spec join(Parts, Separator) -> binary() when
    Parts :: [binary()],
    Separator :: binary().
join([], _Sep) ->
  <<>>;
join([Part], _Sep) ->
  Part;
join(List, Sep) ->
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

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 72
%% coding: latin-1
%% End:
