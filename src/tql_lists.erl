-module(tql_lists).

%% API

-export([ all/1
        , any/1
        , droplast_n/2
        , intersperse/2
        , shuffle/1
        , some/1
        , take/2
        , uniq/1
        , zip4/4
        , zip5/5
        ]).

%%%---------------------------------------------------------------------
%%% API
%%%---------------------------------------------------------------------

-spec all([boolean()]) -> boolean().
all(Xs) ->
  lists:all(fun tql:id/1, Xs).

-spec any([boolean()]) -> boolean().
any(Xs) ->
  lists:any(fun tql:id/1, Xs).

%% TODO: -spec
droplast_n(L = [], _) ->
  L;
droplast_n(L, 0) ->
  L;
droplast_n(L, N) ->
  droplast_n(lists:droplast(L), N - 1).

%% TODO: -spec
intersperse(_S, L = [_]) ->
  L;
intersperse(S, [X | Xs]) ->
  [X, S | intersperse(S, Xs)].

%% TODO: -spec
shuffle(L) ->
  [X || {_, X} <- lists:sort([{rand:uniform(), X} || X <- L])].

-spec some([boolean()]) -> boolean().
some(Xs) ->
  lists:member(true, Xs).

%% TODO: -spec
take(0, _) ->
  [];
take(_, []) ->
  [];
take(N, [X | Xs]) ->
  [X | take(N-1, Xs)].

-spec uniq([A]) -> [A].
uniq(L) ->
  sets:to_list(sets:from_list(L)).


zip4([X | Xs], [Y | Ys], [Z | Zs], [V | Vs]) ->
  [{X, Y, Z, V} | zip4(Xs, Ys, Zs, Vs)];
zip4([], [], [], []) ->
  [].

%% TODO: -spec
zip5([X | Xs], [Y | Ys], [Z | Zs], [V | Vs], [W | Ws]) ->
  [{X, Y, Z, V, W} | zip5(Xs, Ys, Zs, Vs, Ws)];
zip5([], [], [], [], []) ->
  [].

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 72
%% coding: latin-1
%% End:
