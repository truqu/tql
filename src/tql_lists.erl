-module(tql_lists).

%% API

-export([ all/1
        , any/1
        , droplast_n/2
        , intersperse/2
        , shuffle/1
        , take/2
        , uniq/1
        ]).

%%%---------------------------------------------------------------------
%%% API
%%%---------------------------------------------------------------------

%% @doc Conjunction of a list of booleans. Returns `true' if and only if
%% all the booleans are `true'.
-spec all([boolean()]) -> boolean().
all(Xs) ->
  lists:all(fun tql:id/1, Xs).

%% @doc Disjunction of a list of booleans. Returns `true' if at least
%% one of the booleans is `true'.
-spec any([boolean()]) -> boolean().
any(Xs) ->
  lists:any(fun tql:id/1, Xs).

%% @doc Drops the last `N' entries from the given list.
-spec droplast_n(N :: non_neg_integer(), [X]) -> [X].
droplast_n(_, L = []) ->
  L;
droplast_n(0, L) ->
  L;
droplast_n(N, L) ->
  droplast_n(N - 1, lists:droplast(L)).

%% @doc Place the given value between all members of the given list.
-spec intersperse(X, [X]) -> [X].
intersperse(_S, L = [_]) ->
  L;
intersperse(S, [X | Xs]) ->
  [X, S | intersperse(S, Xs)].

%% @doc Shuffle a list.
shuffle(L) ->
  [X || {_, X} <- lists:sort([{rand:uniform(), X} || X <- L])].

%% @doc Take the first `N' elements from the given list.
-spec take(N :: non_neg_integer(), [X]) -> [X].
take(0, _) ->
  [];
take(_, []) ->
  [];
take(N, [X | Xs]) ->
  [X | take(N-1, Xs)].

%% @doc Returns only unique elements from the given list, filtering out
%% duplicates.
%%
%% Elements are considered to be different if they do not match (`=:=').
-spec uniq([A]) -> [A].
uniq(L) ->
  sets:to_list(sets:from_list(L)).

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 72
%% coding: latin-1
%% End:
