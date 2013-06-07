%%%-------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) 2013, Eric Merritt
%%% @doc
%%% Provides the main interface to the system
%%% @end
%%%-------------------------------------------------------------------
-module(episcina).

-export([start_pools/1, start_pool/5, start_pool/1,
         stop/1,
         get_connection/1, get_connection/2,
         return_connection/2]).

%%%===================================================================
%%% Types
%%%===================================================================
-export_type([name/0,
              connection/0,
              connect_fun/0,
              close_fun/0]).

-type name() :: binary() | atom().
-type connection() :: pid() | term().
-type connect_fun() :: fun(() -> term()).
-type close_fun() :: fun((term()) -> ok).

%%%===================================================================
%%% API functions
%%%===================================================================
-spec start_pool(name(),
                 Size::non_neg_integer(),
                 Timeout::non_neg_integer(),
                 connect_fun(),
                 close_fun()) ->
                        {ok, pid()} | {error, term()}.
start_pool(Name, Size, Timeout, ConnectFun, CloseFun) ->
    epna_sup:start_pool(Name, Size, Timeout,
                        ConnectFun, CloseFun).

-spec start_pools([term()]) -> [{ok, pid()} | {error, term()}].
start_pools(Opts) ->
    lists:map(fun start_pool/1, Opts).

-spec start_pool({name(), proplists:proplist()}) ->
                        {ok, pid()} | {error, term()}.
start_pool({Name, Opts}) ->
    Size = proplists:get_value(size, Opts),
    Timeout = proplists:get_value(timeout, Opts),
    ConnectFun = get_connect_fun(Opts),
    CloseFun = get_close_fun(Opts),
    start_pool(Name, Size, Timeout, ConnectFun, CloseFun).


%% @doc Stop the pool, close all db connections
-spec stop(name()) -> ok.
stop(Name) ->
    epna_pool:stop(Name).

%% @doc Get a db connection, wait at most 10 seconds before giving up.
-spec get_connection(name()) -> connection().
get_connection(Name) ->
    epna_pool:get_connection(Name).

%% @doc Get a db connection, wait at most Timeout seconds before
%% giving up.
-spec get_connection(name(), non_neg_integer()) -> connection().
get_connection(Name, Timeout) ->
    epna_pool:get_connection(Name, Timeout).

%% @doc Return a db connection back to the connection pool.
-spec return_connection(name(), connection()) -> ok.
return_connection(Name, C) ->
    epna_pool:return_connection(Name, C).

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec get_connect_fun(proplists:proplist()) -> connect_fun().
get_connect_fun(Opts) ->
    {Module, Fun, Args} = proplists:get_value(connect_provider, Opts),
    fun() ->
            erlang:apply(Module, Fun, Args)
    end.

-spec get_close_fun(proplists:proplist()) -> close_fun().
get_close_fun(Opts) ->
    {Module, Fun, Args} = proplists:get_value(close_provider, Opts),
    fun(Connection) ->
            erlang:apply(Module, Fun, [Connection | Args])
    end.
