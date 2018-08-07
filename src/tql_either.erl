-module(tql_either).

%% API exports
-export([ fold/2
        , sequence/1
        , is_ok/1
        , is_error/1
        , from_bool/3
        , oks/1
        ]).

-type either(Result, Reason) :: {ok, Result} | {error, Reason}.

-export_types([ either/2
              ]).

%%%-----------------------------------------------------------------------------
%%% API
%%%-----------------------------------------------------------------------------

%% @doc Fold over a term with a list of functions.
%%
%% The first function in the list is called with the initial value, and
%% expected to produce either an {@type either(Result, Reason)} or a bare
%% `Result'. If the produced value is an `{ok, Result}' tuple, the
%% next function is called with `Result' as its input. If the produced
%% value is a bare `Result', the next function is called with that
%% value as its input. If the produced value is an error, processing
%% stops and returns that `{error, Reason}' tuple.
%%
%% Note that this function will always produce a tuple, so if the final
%% function produces a bare value, this will be wrapped in a tuple, too.
-spec fold(term(), [fun((Result) -> Return)]) -> either(Result, Reason) when
    Result :: term(),
    Reason :: term(),
    Return :: either(Result, Reason) | Result.
fold(Init, Fs) when is_list(Fs) ->
  Result = lists:foldl(fun fold_handle/2, Init, Fs),
  fold_create(Result).

%% @doc Combine a list of result tuples.
%%
%% This will result in either an `{error, Reason}' if any of the supplied
%% tuples is an error, or `{ok, [Result]}' with all the ok-values sequenced
%% into a list.
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

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 72
%% coding: latin-1
%% End:
