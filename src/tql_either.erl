-module(tql_either).

%% API exports
-export([ fold/2
        , sequence/1
        , is_ok/1
        , is_error/1
        ]).

-type either(Term, Error) :: {ok, Term} | {error, Error}.

-export_types([ either/2
              ]).

%%%---------------------------------------------------------------------
%%% API
%%%---------------------------------------------------------------------


%% @doc Fold over a term with a list of functions.
%%
%% The first function in the list is called with the initial value, and
%% expected to produce either an {@type either(NewValue, Error)} or a bare
%% `NewValue'. If the produced value is an `{ok, NewValue}' tuple, the
%% next function is called with `NewValue' as its input. If the produced
%% value is a bare `NewValue', the next function is called with that
%% value as its input. If the produced value is an error, processing
%% stops and returns that `{error, E}' tuple.
%%
%% Note that this function will always produce a tuple, so if the final
%% function produces a bare value, this will be wrapped in a tuple, too.
-spec fold(term(), [fun((Term) -> Return)]) -> either(Term, Error) when
    Term   :: term(),
    Error  :: term(),
    Return :: either(Term, Error) | Term.
fold(Init, Fs) when is_list(Fs) ->
  Result = lists:foldl(fun fold_handle/2, Init, Fs),
  fold_create(Result).

%% @doc Combine a list of result tuples.
%%
%% This will result in either an `{error, E}' if any of the supplied
%% tuples is an error, or `{ok, [V]}' with all the ok-values sequenced
%% into a list.
-spec sequence([either(Term, Error)]) -> either([Term], Error) when
    Term  :: term(),
    Error :: term().
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
-spec is_ok(either(T :: term(), E :: term())) -> boolean().
is_ok({ok, _}) ->
  true;
is_ok({error, _}) ->
  false.

%% @doc Check whether the given result tuple is of the form `{error, E}'.
-spec is_error(either(T :: term(), E :: term())) -> boolean().
is_error({ok, _}) ->
  false;
is_error({error, _}) ->
  true.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

-spec fold_handle(fun((Term) -> Return), Return) -> Return when 
    Term   :: term(),
    Error  :: term(),
    Return :: either(Term, Error) | Term.
fold_handle(F, {ok, Value}) ->
  F(Value);
fold_handle(_, {error, Reason}) ->
  {error, Reason};
fold_handle(F, Value) ->
  F(Value).

-spec fold_create(either(Term, Error) | Term) -> either(Term, Error) when
    Term  :: term(),
    Error :: term().
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
