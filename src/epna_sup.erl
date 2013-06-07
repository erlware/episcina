%%%-------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) Erlware, LLC.
-module(epna_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_pool/5]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec start_pool(episcina:name(),
                 Size::non_neg_integer(),
                 Timeout::non_neg_integer(),
                 episcina:connect_fun(),
                 episcina:close_fun()) ->
                        {ok, pid()} | {error, term()}.
start_pool(Name, Size, Timeout, ConnectFun, CloseFun) ->
    supervisor:start_child(?MODULE, [Name, Size, Timeout, ConnectFun, CloseFun]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%@private
init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    Pool = {epna_pool, {epna_pool, start_link, []},
              Restart, Shutdown, Type, [epna_pool]},

    {ok, {SupFlags, [Pool]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
