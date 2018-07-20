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

%%%---------------------------------------------------------------------
%%% API
%%%---------------------------------------------------------------------

-spec by(fun ((A) -> B)) -> comparator(A) when
    A :: term(),
    B :: term().
by(X) ->
  by(X, ascending).

-spec by(fun ((A) -> B), order()) -> comparator(A) when
    A :: term(),
    B :: term().
by(Extractor, ascending) ->
  fun (X, Y) -> Extractor(X) =< Extractor(Y) end;
by(Extractor, descending) ->
  fun (X, Y) -> Extractor(Y) =< Extractor(X) end.

-spec reverse(comparator(T)) -> comparator(T) when T :: term().
reverse(Comparator) ->
  fun (X, Y) -> Comparator(Y, X) end.

-spec by_prop(Key :: term()) -> comparator(map()).
by_prop(Key) ->
  by_prop(Key, ascending).

-spec by_prop(Key :: term(), order()) -> comparator(map()).
by_prop(Key, Order) ->
  by(fun (Map) -> maps:get(Key, Map) end, Order).

-spec by_prop(Key, Default, order()) -> comparator(map()) when
    Key     :: term(),
    Default :: term().
by_prop(Key, Default, Order) ->
  by(fun (Map) -> maps:get(Key, Map, Default) end, Order).

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
