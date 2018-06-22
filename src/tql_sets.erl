-module(tql_sets).

%% API

-export([ all/2
        , equals/2
        , intersects/2
        , is_empty/1
        , map/2
        , symmetric_difference/2
        ]).

%%%---------------------------------------------------------------------
%%% API
%%%---------------------------------------------------------------------

%% @doc Check if all entries of a set satisfy a predicate.
%%
%% Beware vacuous truths: if the given set is empty, all entries match
%% the predicate and the result will be `true'.
-spec all(fun((T) -> boolean()), sets:set(T)) -> boolean().
all(Pred, Set) ->
  lists:all(Pred, sets:to_list(Set)).

%% @doc Check if 2 sets contain the same elements.
-spec equals(sets:set(T), sets:set(T)) -> boolean().
equals(Set1, Set2) ->
  sets:is_subset(Set1, Set2) andalso sets:is_subset(Set2, Set1).

%% @doc Check if the given sets have a nonempty intersection.
-spec intersects(sets:set(T), sets:set(T)) -> boolean().
intersects(Set1, Set2) ->
  not is_empty(sets:intersection(Set1, Set2)).

%% @doc Check if the given set is empty.
-spec is_empty(sets:set(T :: term())) -> boolean().
is_empty(Set) ->
  sets:size(Set) == 0.

%% @doc Map a function over the elements of a set.
%%
%% Note that if some of the values in the given set map to the same
%% value, the resulting set may be smaller as it will only contain unique
%% elements.
-spec map(fun((T1) -> T2), sets:set(T1)) -> sets:set(T2).
map(Fun, Set) ->
  sets:fold( fun(X, Acc) -> sets:add_element(Fun(X), Acc) end
           , sets:new()
           , Set
           ).

%% @doc Calculates the symmetric difference of 2 sets.
%%
%% In other words, find all the elements that do not exist in both sets.
-spec symmetric_difference(sets:set(T), sets:set(T)) -> sets:set(T).
symmetric_difference(Set1, Set2) ->
  sets:union(sets:subtract(Set1, Set2), sets:subtract(Set2, Set1)).

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 72
%% coding: latin-1
%% End:
