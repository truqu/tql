-module(tql).

%% API exports
-export([ binary_join/2
        , id/1
        , to_hex/1
        , pipe/2
        , either/1
        , either/2
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

pipe(Arg, Fs) ->
  (tql_fun:compose(lists:reverse(Fs)))(Arg).

-spec either([fun((Term) -> Return)])
            -> {ok, Term} | {error, Error}
                 when Term   :: term()
                    , Error  :: term()
                    , Return :: Term | {ok, Term} | {error, Error}.
either([Head | Tail]) ->
  either(Head(), Tail).

-spec either(term(), [fun((Term) -> Return)])
            -> {ok, Term} | {error, Error}
                 when Term   :: term()
                    , Error  :: term()
                    , Return :: Term | {ok, Term} | {error, Error}.
either(Init, Fs) when is_list(Fs) ->
  Result = lists:foldl(fun either_fold/2, Init, Fs),
  either_create(Result).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

-spec either_fold(fun((Term) -> Return), Return)
                 -> {ok, Return} | Return | {error, Error}
                      when Term   :: term()
                         , Error  :: term()
                         , Return :: Term | {ok, Term} | {error, Error}.
either_fold(F, {ok, Value}) ->
  F(Value);
either_fold(_, {error, Reason}) ->
  {error, Reason};
either_fold(F, Value) ->
  F(Value).

-spec either_create({error, term()} | {ok, term()} | term()) ->
  {error, term()} | {ok, term()}.
either_create({error, Reason}) ->
  {error, Reason};
either_create({ok, Value}) ->
  {ok, Value};
either_create(Value) ->
  {ok, Value}.


%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 72
%% coding: latin-1
%% End:
