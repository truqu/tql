-module(tql_compare).

%% API exports
-export([ by/1
        , by/2
        , by_prop/1
        , by_prop/2
        , by_prop/3
        , concat/1
        , reverse/1
        ]).

-type comparator(T) :: fun ((T, T) -> boolean()).
-type order() :: ascending | descending.

-export_types([ comparator/1
              , order/0
              ]).

%%%---------------------------------------------------------------------
%%% API
%%%---------------------------------------------------------------------

%% @equiv by(F, ascending)
-spec by(fun ((A) -> B)) -> comparator(A) when
    A :: term(),
    B :: term().
by(F) ->
  by(F, ascending).

%% @doc Creates a comparison function (`comparator(A)') compatible with
%% `lists:sort/2', using the extracted term to sort in the given
%% direction.
-spec by(fun ((A) -> B), order()) -> comparator(A) when
    A :: term(),
    B :: term().
by(Extractor, ascending) ->
  fun (X, Y) -> Extractor(X) =< Extractor(Y) end;
by(Extractor, descending) ->
  fun (X, Y) -> Extractor(Y) =< Extractor(X) end.

%% @doc Reverses the order in which elements are sorted by a comparator.
-spec reverse(comparator(T)) -> comparator(T) when T :: term().
reverse(Comparator) ->
  fun (X, Y) -> Comparator(Y, X) end.

%% @equiv by_prop(Key, ascending)
-spec by_prop(Key :: term()) -> comparator(map()).
by_prop(Key) ->
  by_prop(Key, ascending).

%% @doc Creates a comparator for maps, sorting by a given property, in
%% the given direction.
-spec by_prop(Key :: term(), order()) -> comparator(map()).
by_prop(Key, Order) ->
  by(fun (Map) -> maps:get(Key, Map) end, Order).

%% @doc Creates a comparator for maps, sorting by the given property and
%% using the `Default' as fallback value, to sort in the provided
%% direction.
-spec by_prop(Key, Default, order()) -> comparator(map()) when
    Key     :: term(),
    Default :: term().
by_prop(Key, Default, Order) ->
  by(fun (Map) -> maps:get(Key, Map, Default) end, Order).

%% @doc Composes multiple comparators together.
%%
%% Fallthrough happens when 2 items are considered equivalent _according
%% to the comparators_. Specifically, this means when `Compare(A, B) ==
%% Compare(B, A)'.
-spec concat([Comp, ...]) -> Comp when
    Comp :: comparator(T :: term()).
concat(Comparators) ->
  fun (X, Y) ->
      concat_help(X, Y, Comparators)
  end.

%%%---------------------------------------------------------------------
%%% Internal functions
%%%---------------------------------------------------------------------

-spec concat_help(X, Y, [Comp, ...]) -> boolean() when
    X    :: T,
    Y    :: T,
    Comp :: comparator(T),
    T    :: term().
concat_help(X, Y, [Comp]) ->
  Comp(X, Y);
concat_help(X, Y, [Comp | Rest]) ->
  OtherDir = Comp(Y, X),
  case Comp(X, Y) of
    OtherDir -> concat_help(X, Y, Rest);
    Res      -> Res
  end.

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 72
%% coding: latin-1
%% End:
