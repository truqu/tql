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

compose(Fs) when is_list(Fs) ->
  lists:foldr(fun compose/2, fun tql:id/1, Fs).

compose(F, G) ->
  fun (X) -> F(G(X)) end.

-spec all([fun ((A) -> boolean())]) -> fun ((A) -> boolean()).
all(Fs) ->
  fun (X) -> tql_lists:all(sequence(Fs, X)) end.

-spec any([fun ((A) -> boolean())]) -> fun ((A) -> boolean()).
any(Fs) ->
  fun (X) -> tql_lists:any(sequence(Fs, X)) end.

-spec negate(fun ((A) -> boolean())) -> fun ((A) -> boolean()).
negate(F) ->
  fun (X) -> not F(X) end.

sequence(Fs, X) ->
  sequence(Fs, X, []).

%%%---------------------------------------------------------------------
%%% Internal functions
%%%---------------------------------------------------------------------

sequence([], _, Ys) ->
  lists:reverse(Ys);
sequence([F | Fs], X, Ys) ->
  sequence(Fs, X, [F(X) | Ys]).

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 72
%% coding: latin-1
%% End:
