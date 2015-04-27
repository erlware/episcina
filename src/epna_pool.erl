%%%-------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) Erlware, LLC.
%%% @doc
%%%  Provides the core pool implementation.
%%% @end
%%%-------------------------------------------------------------------
-module(epna_pool).

%% API
-export([start_link/5, stop/1,
         get_connection/1, get_connection/2, return_connection/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-ifdef(namespaced_types).
-record(state, {id :: binary(),
                size :: non_neg_integer(),
                connections :: [{Connection::term(), Time::non_neg_integer()}],
                working :: dict:dict(),
                waiting :: queue:queue(),
                connect_fun :: episcina:connect_fun(),
                close_fun :: episcina:close_fun(),
                timeout :: non_neg_integer(),
                timer :: timer:tref()}).
-else.
-record(state, {id :: binary(),
                size :: non_neg_integer(),
                connections :: [{Connection::term(), Time::non_neg_integer()}],
                working :: dict(),
                waiting :: queue(),
                connect_fun :: episcina:connect_fun(),
                close_fun :: episcina:close_fun(),
                timeout :: non_neg_integer(),
                timer :: timer:tref()}).
-endif.

%%%===================================================================
%%% Types
%%%===================================================================

-type state() :: record(state).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc This function starts up a new pool and registers it by
%% name. That name will be used in future calls to interact with the
%% pool.
%%
%% @param Name The name of the pool
%% @param Size The maximum size of the pool. After this no further
%% connections will be created and callers will have to wait for a
%% connection to be freed.
%% @param Timeout The maximum amount of time a caller is allowed to
%% keep a connection before that connection is closed and considered
%% invalid.
%% @param ConnectFun A function that takes no arguments but returns a
%% connection. This is used to create new connections in the system
%% @param CloseFun A function that takes a connection as an argument
%% and returns ok. This is expected to close the connection
%% gracefully.
-spec start_link(episcina:name(),
                 Size::non_neg_integer(),
                 Timeout::non_neg_integer(),
                 episcina:connect_fun(),
                 episcina:close_fun()) ->
                        {ok, pid()} | {error, term()}.
start_link(Name, Size, Timeout, ConnectFun, CloseFun) ->
    gen_server:start_link(?MODULE, {Name, Size, Timeout, ConnectFun, CloseFun},
                          []).
-spec stop(episcina:name()) -> ok.
stop(Name) ->
    {Pid, _} = gproc:await(make_registered_name(Name)),
    gen_server:cast(Pid, stop).

-spec get_connection(episcina:name()) -> episcina:connection().
get_connection(Name) ->
    get_connection(Name, 10000).

-spec get_connection(episcina:name(), non_neg_integer()) -> episcina:connection().
get_connection(Name, Timeout) ->
    {Pid, _} = gproc:await(make_registered_name(Name), Timeout),
    try
        gen_server:call(Pid, get_connection, Timeout)
    catch
        _:_ ->
            gen_server:cast(Pid, {cancel_wait, erlang:self()}),
            {error, timeout}
    end.

return_connection(Name, C) ->
    {Pid, _} = gproc:await(make_registered_name(Name)),
    gen_server:cast(Pid, {return_connection, C}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init({Name, Size, Timeout, ConnectFun, CloseFun}) ->
    erlang:process_flag(trap_exit, true),
    Id = case Name of
             undefined ->
                 erlang:self();
             _Name -> Name
         end,
    {ok, Connection} = connect(ConnectFun),
    {ok, TRef} = timer:send_interval(60000, close_unused),
    State = #state{id = Id,
                   size = Size,
                   connections = [{Connection, now_secs()}],
                   working = dict:new(),
                   timeout = Timeout,
                   connect_fun = ConnectFun,
                   close_fun = CloseFun,
                   waiting = queue:new(),
                   timer = TRef},
    gproc:reg(make_registered_name(Name), erlang:self()),
    {ok, State}.

%% @private
handle_call(get_connection, From,
            #state{connections = Connections,
                   waiting = Waiting,
                   connect_fun = ConnectFun} = State) ->
    %% Requestor wants a connection. When available then immediately
    %% return, otherwise add to the waiting queue.
    case Connections of
        [{C,_} | T] ->
            %% Return existing unused connection
            {noreply, deliver(From, C, State#state{connections = T})};
        [] ->
            case dict:size(State#state.working) < State#state.size of
                true ->
                    %% Allocate a new connection and return it.
                    {ok, C} = connect(ConnectFun),
                    {noreply, deliver(From, C, State)};
                false ->
                    %% Reached max connections, let the requestor wait
                    {noreply, State#state{waiting = queue:in(From, Waiting)}}
            end
    end;
handle_call(Request, _From, State) ->
    %% Trap unsupported calls
    {stop, {unsupported_call, Request}, State}.

%% @private
handle_cast({return_connection, C}, State0) ->
    %% Connection returned from the requestor, back into our pool.
    State1 = cleanup_connection(C, State0),
    {noreply, return(C, State1)};
handle_cast({cancel_wait, Pid}, State = #state{waiting = Waiting}) ->
    %% Requestor gave up (timeout), remove from our waiting queue (if any).
    Waiting2 = queue:filter(fun({QPid, _Tag}) -> QPid =/= Pid end, Waiting),
    {noreply, State#state{waiting = Waiting2}};
handle_cast(stop, State) ->
    %% Stop the connections pool.
    {stop, normal, State};
handle_cast(Request, State) ->
    {stop, {unsupported_cast, Request}, State}.

%% @private
handle_info(close_unused, State = #state{close_fun = CloseFun}) ->
    %% Close all connections that are unused for longer than a minute.
    Old = now_secs() - 60,
    {Unused, Used} = lists:partition(fun({_C,Time}) ->
                                             Time < Old
                                     end,
                                     State#state.connections),
    lists:foreach(fun({C,_}) ->
                          CloseFun(C)
                  end, Unused),
    {noreply, State#state{connections=Used}};
handle_info({connection_timeout, C}, State0 = #state{close_fun=CloseFun,
                                                     working=Working}) ->
    %% We got a connection timeout. So lets remove the connection
    %% stuff and close the connection
    case dict:find(C, Working) of
        error ->
            {noreply, State0};
        {ok, {_TRef, Pid}} ->
            State1 = cleanup_connection(C, State0),
            %% The connection is no longer good, the process running
            %% the connection shouldn't do anything else with it. So
            %% we go ahead and tell it to exit with a reasonable
            %% message.
            erlang:exit(Pid, connection_timeout),
            ok = CloseFun(C),
            {noreply, State1}
        end;

handle_info({'EXIT', Pid, _Reason},
            State0 = #state{connections = Connections0}) ->
    State1 = case proplists:is_defined(Pid, Connections0) of
                 true ->
                     connection_exit(Pid, Connections0, State0);
                 false ->
                     external_owner_exit(Pid, State0)
             end,
        {noreply, State1};
handle_info(Info, State) ->
    %% Trap unsupported info calls.
    {stop, {unsupported_info, Info}, State}.

%% @private
terminate(_Reason, State) ->
        timer:cancel(State#state.timer),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    State.

%%%===================================================================
%%% Internal functions
%%%===================================================================
external_owner_exit(Pid, State0 = #state{close_fun = CloseFun,
                                         working = Working}) ->
    %% Requestor we are monitoring went down. Kill the associated
    %% connection, as it might be in an unknown state.
    dict:fold(fun(C, {_, PossiblePid}, State1) ->
                      case Pid of
                          PossiblePid ->
                              CloseFun(C),
                              cleanup_connection(C, State1);
                          _ ->
                              State1
                      end;
                 (_, _, State1) ->
                      State1
              end, State0, Working).


connection_exit(ConnectionPid, Connections0, State0) ->
    %% One of our database connections went down. Clean up our
    %% administration.
    Connections1 = proplists:delete(ConnectionPid, Connections0),
    State1 = cleanup_connection(ConnectionPid, State0),
    State1#state{connections = Connections1}.


-spec deliver({pid(), term()}, episcina:connection(), state()) -> state().
deliver(From = {Pid, _}, C, State = #state{working=Working, timeout=Timeout}) ->
    erlang:link(Pid),
    gen_server:reply(From, {ok, C}),
    {ok, Tref} = timer:send_after(Timeout, {connection_timeout, C}),
    State#state{working=dict:store(C, {Tref, Pid}, Working)}.

-spec return(episcina:connection(), state()) -> state().
return(C, State0 = #state{connections = Connections, waiting = Waiting}) ->
    State1 = cleanup_connection(C, State0),
    case proplists:is_defined(C, Connections) of
        true ->
            %% Its been returned twice, nothing to do here then
            State1;
        false ->
            case queue:out(Waiting) of
                {{value, From}, Waiting2} ->
                    State2 = deliver(From, C, State1),
                    State2#state{waiting = Waiting2};
                {empty, _Waiting} ->
                    Connections2 = [{C, now_secs()} | Connections],
                    State1#state{connections = Connections2}
            end
    end.

-spec cleanup_connection(episcina:connection(), state()) -> state().
cleanup_connection(C, State = #state{working=Working}) ->
    case dict:find(C, Working) of
        error ->
            State;
        {ok, {TimeoutRef, Pid}} ->
            erlang:unlink(Pid),
            timer:cancel(TimeoutRef),
            State#state{working = dict:erase(C, Working)}
    end.

%% Return the current time in seconds, used for timeouts.
-spec now_secs() -> non_neg_integer().
now_secs() ->
    {M,S,_M} = erlang:now(),
    M*1000 + S.

-spec make_registered_name(episcina:name()) -> term().
make_registered_name(Name) ->
    {n, l, {epna_pool, Name}}.

-spec connect(episcina:connect_fun()) -> episcina:connection().
connect(Fun) ->
    Result = {ok, C} = Fun(),
    if
        erlang:is_pid(C) ->
            erlang:link(C)
    end,
    Result.
