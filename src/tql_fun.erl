-module(tql_fun).

%% API

-export([ compose/1
        , compose/2
        ]).

%%%---------------------------------------------------------------------
%%% API
%%%---------------------------------------------------------------------

compose(Fs) when is_list(Fs) ->
    lists:foldr(fun compose/2, fun tql:id/1, Fs).

compose(F, G) ->
    fun (X) -> F(G(X)) end.

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 72
%% coding: latin-1
%% End:
