-module(tql_fun).

%% API

-export([ compose/1
        , compose/2
        , all/1
        , any/1
        , negate/1
        , sequence/2
        ]).

%%%---------------------------------------------------------------------
%%% API
%%%---------------------------------------------------------------------

%% @doc Compose many functions.
%%
%% Given inputs `[F1, F2, ..., Fn]', returns <code>F1 &#8728; F2 &#8728;
%% ... &#8728; Fn</code>. Note that function composition is associative,
%% but not commutative. As such, keep in mind that functions are
%% executed last to first.
-spec compose([Fun]) -> Fun when Fun :: fun ((A :: term()) -> B :: term()).
compose(Fs) when is_list(Fs) ->
  lists:foldr(fun compose/2, fun tql:id/1, Fs).

%% @doc Compose 2 functions.
%%
%% Given functions `F' and `G', returns a <code>F &#8728; G</code>.
%%
%% ```
%%    F = fun (X) -> X * 2 end,
%%    G = fun (X) -> X + 1 end,
%%    H1 = compose(F, G),
%%    H2 = compose(G, F),
%%    10 = H1(4),
%%    9 = H2(4).
%% '''
-spec compose(fun((A) -> B), fun((B) -> C)) -> fun ((A) -> C).
compose(F, G) ->
  fun (X) -> F(G(X)) end.

%% @doc Compose a bunch of predicates using logical conjunction.
%%
%% For any given `X', the resulting function will return `true' if and
%% only if all of the specified functions would return `true' for that
%% same input. Beware vacuous truths: if the list of predicates is
%% empty, "all" predicates return `true'.
-spec all([fun ((A) -> boolean())]) -> fun ((A) -> boolean()).
all(Fs) ->
  fun (X) -> tql_lists:all(sequence(Fs, X)) end.

%% @doc Compose a bunch of predicates using logical disjunction.
%%
%% For any given `X', the resulting function will return `true' if at
%% least one of the specified predicates would return `true'.
-spec any([fun ((A) -> boolean())]) -> fun ((A) -> boolean()).
any(Fs) ->
  fun (X) -> tql_lists:any(sequence(Fs, X)) end.

%% @doc Logical negation for a predicate.
-spec negate(fun ((A) -> boolean())) -> fun ((A) -> boolean()).
negate(F) ->
  fun (X) -> not F(X) end.

%% @doc Given a list of functions and an input, return a list where each
%% element represents the output of running the matching function on the
%% input.
-spec sequence([Fun], A) -> [B] when Fun :: fun ((A) -> B).
sequence(Fs, X) ->
  lists:map(fun(F) -> F(X) end, Fs).

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 72
%% coding: latin-1
%% End:
