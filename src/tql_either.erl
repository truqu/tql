-module(tql_either).

%% API exports
-export([ fold/1
        , fold/2
        ]).

%%%---------------------------------------------------------------------
%%% API
%%%---------------------------------------------------------------------

-spec fold([fun((Term) -> Return)])
            -> {ok, Term} | {error, Error}
                 when Term   :: term()
                    , Error  :: term()
                    , Return :: Term | {ok, Term} | {error, Error}.
fold([Head | Tail]) ->
  fold(Head(), Tail).

-spec fold(term(), [fun((Term) -> Return)])
            -> {ok, Term} | {error, Error}
                 when Term   :: term()
                    , Error  :: term()
                    , Return :: Term | {ok, Term} | {error, Error}.
fold(Init, Fs) when is_list(Fs) ->
  Result = lists:foldl(fun fold_handle/2, Init, Fs),
  fold_create(Result).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

-spec fold_handle(fun((Term) -> Return), Return)
                 -> {ok, Return} | Return | {error, Error}
                      when Term   :: term()
                         , Error  :: term()
                         , Return :: Term | {ok, Term} | {error, Error}.
fold_handle(F, {ok, Value}) ->
  F(Value);
fold_handle(_, {error, Reason}) ->
  {error, Reason};
fold_handle(F, Value) ->
  F(Value).

-spec fold_create({error, term()} | {ok, term()} | term()) ->
  {error, term()} | {ok, term()}.
fold_create({error, Reason}) ->
  {error, Reason};
fold_create({ok, Value}) ->
  {ok, Value};
fold_create(Value) ->
  {ok, Value}.

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 72
%% coding: latin-1
%% End:
