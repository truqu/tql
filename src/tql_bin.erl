-module(tql_bin).

%% API

-export([ to_hex/1
        , join/2
        , trim/1
        , trim_left/1
        , trim_right/1
        , reverse/1
        ]
       ).

-define(TRIM_CHARACTERS, [<<"\s">>, <<"\t">>, <<"\n">>, <<"\r">>]).

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

%% @doc Trims a binary of all whitespace.
%% Trimmed Characters: \s, \t, \n, \r
%% ```
%%    V = tql_bin:trim(<<" test ">>),
%%    V = <<"test">>.
%% '''
-spec trim(binary()) -> binary().
trim(Binary) when is_binary(Binary) ->
  tql:pipe( Binary
          , [ fun trim_right/1
            , fun trim_left/1
            ]).

%% @doc Trims a binary of all ending whitespace.
%% Trimmed Characters: \s, \t, \n, \r
%%
%% ```
%%    V = tql_bin:trim_right(<<" test ">>),
%%    V = <<" test">>.
%% '''
-spec trim_right(binary()) -> binary().
trim_right(Binary) ->
  tql:pipe( Binary
          , [ fun reverse/1
            , fun trim_left/1
            , fun reverse/1
            ]).

%% @doc Trims a binary of all leading whitespace.
%% Trimmed Characters: \s, \t, \n, \r
%%
%% ```
%%    V = tql_bin:trim_left(<<" test ">>),
%%    V = <<"test ">>.
%% '''
-spec trim_left(binary()) -> binary().
trim_left(<<H:1/binary, Rest/binary>> = Binary) ->
  case lists:member(H, ?TRIM_CHARACTERS) of
    true ->
      trim_left(Rest);
    false ->
      Binary
  end.

%% @doc Reverses the order of characters in a binary.
%%
%% ```
%%    V = tql_bin:reverse(<<"reverse">>),
%%    V = <<"esrever">>.
%% '''
-spec reverse(binary()) -> binary().
reverse(Binary) ->
  reverse(Binary, <<>>).

-spec reverse(binary(), binary()) -> binary().
reverse(<<>>, Acc) ->
  Acc;
reverse(<<H:1/binary, Rest/binary>>, Acc) ->
  reverse(Rest, <<H/binary, Acc/binary>>).

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 72
%% coding: latin-1
%% End:
