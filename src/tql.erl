-module(tql).

%% API exports
-export([ id/1
        , pipe/2
        ]).

%%%---------------------------------------------------------------------
%%% API
%%%---------------------------------------------------------------------


%% @doc Returns the argument itself.
%%
%% This is useful for composition and as a generic helper.
-spec id(A) -> A.
id(X) ->
  X.

%% @doc Thread a value through a set of unary functions.
%%
%% The first function receives the provided argument as its input, with
%% its output being passed to the next function, and so on. When the
%% provided list of functions is empty, this amounts to `id'.
%%
%% ```
%%    Funs = [fun string:titlecase/1, fun string:reverse/1],
%%    Res = tql:pipe("this is an example", Funs),
%%    Res = "elpmaxe na si sihT".

-spec pipe(Arg, Funs) -> Res when
    Arg :: term(),
    Funs :: [Fun],
    Fun :: fun ((A :: term()) -> B :: term()),
    Res :: term().
pipe(Arg, Fs) ->
  (tql_fun:compose(lists:reverse(Fs)))(Arg).

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 72
%% coding: latin-1
%% End:
