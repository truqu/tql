-module(tql_either).

%% API exports
-export([ fold/1
        , fold/2
        , sequence/1
        , is_ok/1
        , is_error/1
        ]).

%%%---------------------------------------------------------------------
%%% API
%%%---------------------------------------------------------------------

%% @doc Fold functions into a value.
%%
%% The first function is expected to be a nullary function, producing
%% either an `{ok, V}', `{error, E}' or a plain term value. If the
%% result is an `{ok, V}' or plain term, the resulting value is fed to
%% the next function, and so on. As soon as an `{error, E}' tuple is
%% encountered, the resulting error tuple is returned.
-spec fold([fun((Term) -> Return), ...])
          -> {ok, Term} | {error, Error}
               when Term   :: term()
                  , Error  :: term()
                  , Return :: Term | {ok, Term} | {error, Error}.
fold([Head | Tail]) ->
  fold(Head(), Tail).

%% @doc Fold over a term with a list of functions.
%%
%% Much like `tql_either:fold/1' except the first function is called
%% with the given value. If the given list of functions is empty,
%% returns `{ok, V}' where `V' is the given value.
-spec fold(term(), [fun((Term) -> Return)])
          -> {ok, Term} | {error, Error}
               when Term   :: term()
                  , Error  :: term()
                  , Return :: Term | {ok, Term} | {error, Error}.
fold(Init, Fs) when is_list(Fs) ->
  Result = lists:foldl(fun fold_handle/2, Init, Fs),
  fold_create(Result).

%% @doc Combine a list of result tuples.
%%
%% This will result in either an `{error, E}' if any of the supplied
%% tuples is an error, or `{ok, [V]}' with all the ok-values sequenced
%% into a list.
-spec sequence([{error, term()} | {ok, term()}])
              -> {error, term()} | {ok, [term()]}.
sequence(Eithers) ->
  lists:foldr(fun
    ({ok, Success}, {ok, Successes}) ->
      {ok, [Success | Successes]};
    ({ok, _Success}, {error, Failure}) ->
      {error, Failure};
    ({error, Failure}, _) ->
      {error, Failure}
  end, {ok, []}, Eithers).

%% @doc Check whether the given result tuple is of the form `{ok, V}'.
-spec is_ok({ok, term()} | {error, term()}) -> boolean().
is_ok({ok, _}) ->
  true;
is_ok({error, _}) ->
  false.

%% @doc Check whether the given result tuple is of the form `{error, E}'.
-spec is_error({ok, term()} | {error, term()}) -> boolean().
is_error({ok, _}) ->
  false;
is_error({error, _}) ->
  true.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

-spec fold_handle(fun((Term) -> Return), Return)
                 -> {ok, Return} | Return | {error, Error}
                      when Term   :: term()
                         , Error  :: term()
                         , Return :: Term | {ok, Term} | {error, Error}.
fold_handle(F, {ok, Value}) ->
  F(Value);
fold_handle(_, {error, Reason}) ->
  {error, Reason};
fold_handle(F, Value) ->
  F(Value).

-spec fold_create({error, term()} | {ok, term()} | term()) ->
  {error, term()} | {ok, term()}.
fold_create({error, Reason}) ->
  {error, Reason};
fold_create({ok, Value}) ->
  {ok, Value};
fold_create(Value) ->
  {ok, Value}.

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 72
%% coding: latin-1
%% End:
