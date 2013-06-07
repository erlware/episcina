%%%-------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) 2013, Eric Merritt
%%%-------------------------------------------------------------------
-module(epna_app).

-behaviour(application).

-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================
start(_Type, _Args) ->
    Result = epna_sup:start_link(),
    start_pools(),
    Result.

stop(_State) ->
    ok.


%%%===================================================================
%%% Internal Functions
%%%===================================================================
-spec start_pools() -> {ok, pid()} | {error, term()} | ok.
start_pools() ->
    case application:get_env(pools) of
        {ok, Pools} ->
            episcina:start_pools(Pools);
        _ ->
            ok
    end.
