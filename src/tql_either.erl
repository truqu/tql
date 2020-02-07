-module(tql_either).

%% API exports
-export([ fold/2
        , sequence/1
        , traverse/2
        , is_ok/1
        , is_error/1
        , from_bool/3
        , with_default/2
        , and_/1
        , oks/1
        ]).

-type either(Result, Reason) :: {ok, Result} | {error, Reason}.

-export_type([either/2]).

%%%-----------------------------------------------------------------------------
%%% API
%%%-----------------------------------------------------------------------------

%% @doc Fold over a term with a list of functions.
%%
%% The first function in the list is called with the initial value, and expected
%% to produce either an {@type either(Result, Reason)} or a bare `Result'. If
%% the produced value is an `{ok, Result}' tuple, the next function is called
%% with `Result' as its input. If the produced value is a bare `Result', the
%% next function is called with that value as its input. If the produced value
%% is an error, processing stops and returns that `{error, Reason}' tuple.
%%
%% Note that this function will always produce a tuple, so if the final function
%% produces a bare value, this will be wrapped in a tuple, too.
-spec fold(term(), [fun((Result) -> Return)]) -> either(Result, Reason) when
    Result :: term(),
    Reason :: term(),
    Return :: either(Result, Reason) | Result.
fold(Init, Fs) when is_list(Fs) ->
  Result = lists:foldl(fun fold_handle/2, Init, Fs),
  fold_create(Result).

%% @doc Combine a list of result tuples.
%%
%% This will result in either an `{error, Reason}' if any of the supplied tuples
%% is an error, or `{ok, [Result]}' with all the ok-values sequenced into a
%% list.
-spec sequence([either(Result, Reason)]) -> either([Result], Reason) when
    Result :: term(),
    Reason :: term().
sequence(Eithers) ->
  lists:foldr(fun
    ({ok, Success}, {ok, Successes}) ->
      {ok, [Success | Successes]};
    ({ok, _Success}, {error, Failure}) ->
      {error, Failure};
    ({error, Failure}, _) ->
      {error, Failure}
  end, {ok, []}, Eithers).

%% @doc Collect results of applying either-returning function on inputs.
%%
%% We'll bail out with the error on the first item for which the function errors
%% out. If the function returns an ok value for all inputs, the result will
%% contain those values collected in a single result.
-spec traverse(F, [A]) -> either([Result], Reason) when
    F :: fun ((A) -> either(Result, Reason)),
    A :: term(),
    Result :: term(),
    Reason :: term().
traverse(F, Xs) ->
  traverse_help(F, Xs, []).

%% @doc Check whether the given result tuple is of the form `{ok, Result}'.
-spec is_ok(either(Result, Reason)) -> boolean() when
    Result :: term(),
    Reason :: term().
is_ok({ok, _}) ->
  true;
is_ok({error, _}) ->
  false.

%% @doc Check whether the given result tuple is of the form `{error, Reason}'.
-spec is_error(either(Result, Reason)) -> boolean() when
    Result :: term(),
    Reason :: term().
is_error({ok, _}) ->
  false;
is_error({error, _}) ->
  true.

%% @doc Convert a boolean to the form `{ok, Result}' or `{error, Reason}'.
-spec from_bool(Result, Reason, boolean()) -> either(Result, Reason) when
    Result :: term(),
    Reason :: term().
from_bool(Result, _, true) ->
  {ok, Result};
from_bool(_, Reason, false) ->
  {error, Reason}.

%% @doc Fold over a term that depend on a previous result
-spec and_(fun((X) -> either(Y, Err))) -> fun((X) -> either({X, Y}, Err)).
and_(F) ->
  fun (X) ->
      case F(X) of
        {ok, Y} ->
          {ok, {X,Y}};
        {error, E} ->
          {error, E};
        Y ->
          {ok, {X, Y}}
      end
  end.

-spec with_default(either(Result, Reason), Default) -> Result | Default when
    Result  :: term(),
    Reason  :: term(),
    Default :: term().
with_default({ok, Value}, _) ->
  Value;
with_default({error, _}, Default) ->
  Default.

%% @doc Collect the `ok' values from a list of result tuples.
-spec oks([either(Result, Reason)]) -> [Result] when
    Result :: term(),
    Reason :: term().
oks(Eithers) ->
  lists:filtermap( fun({ok, Result}) ->
                       {true, Result};
                      ({error, _}) ->
                       false
                   end
                 , Eithers).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

-spec fold_handle(fun((Result) -> Return), Return) -> Return when
    Result :: term(),
    Reason :: term(),
    Return :: either(Result, Reason) | Result.
fold_handle(F, {ok, Value}) ->
  F(Value);
fold_handle(_, {error, Reason}) ->
  {error, Reason};
fold_handle(F, Value) ->
  F(Value).

-spec fold_create(either(Result, Reason) | Result) -> either(Result, Reason) when
    Result :: term(),
    Reason :: term().
fold_create({error, Reason}) ->
  {error, Reason};
fold_create({ok, Value}) ->
  {ok, Value};
fold_create(Value) ->
  {ok, Value}.

-spec traverse_help(F, Xs, Acc) -> either(Result, Reason) when
    F :: fun ((A) -> either(Result, Reason)),
    Acc :: [Result],
    Xs :: [A],
    A :: term(),
    Result :: term(),
    Reason :: term().
traverse_help(_, [], Acc) ->
  {ok, lists:reverse(Acc)};
traverse_help(F, [X | Xs], Acc) ->
  case fold_create(F(X)) of
    {ok, V} ->
      traverse_help(F, Xs, [V | Acc]);
    {error, Reason} ->
      {error, Reason}
  end.

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 80
%% coding: latin-1
%% End:
