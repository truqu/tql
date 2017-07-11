-module(tql_sets).

%% API

-export([ all/2
        , equal/2
        , intersect/2
        , is_empty/1
        , map/2
        ]).

%%%---------------------------------------------------------------------
%%% API
%%%---------------------------------------------------------------------

-spec all(fun((T) -> boolean()), sets:set(T)) -> boolean().
all(Pred, Set) ->
  lists:all(Pred, sets:to_list(Set)).

-spec equal(sets:set(), sets:set()) -> boolean().
equal(Set1, Set2) ->
  sets:is_subset(Set1, Set2) andalso sets:is_subset(Set2, Set1).

-spec intersect(sets:set(), sets:set()) -> boolean().
intersect(Set1, Set2) ->
  sets:size(sets:intersection(Set1, Set2)) > 0.

-spec is_empty(sets:set()) -> boolean().
is_empty(Set) ->
  sets:size(Set) == 0.

-spec map(fun((T1) -> T2), sets:set(T1)) -> sets:set(T2).
map(Fun, Set) ->
  sets:fold( fun(X, Acc) -> sets:add_element(Fun(X), Acc) end
           , sets:new()
           , Set
           ).

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 72
%% coding: latin-1
%% End:
