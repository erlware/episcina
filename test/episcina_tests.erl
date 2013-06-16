%%%-------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) Erlware, LLC.
%%%-------------------------------------------------------------------
-module(episcina_tests).

-include_lib("eunit/include/eunit.hrl").

-export([get_connections/0, setup/0, teardown/1]).

-define(SIZE, 5).
-define(TIMEOUT, 5000).
-define(POOL, test_pool).

%%%===================================================================
%%% Tests
%%%===================================================================

pool_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     ?_test(
        begin
            get_connections(),
            get_connection_timeout(),
            return_twice(),
            connection_dies(),
            connection_owner_dies()
        end)}.

setup() ->
    application:start(gproc),
    application:start(episcina),
    episcina:start_pool(?POOL, ?SIZE, ?TIMEOUT,
                        fun() -> {ok, proc_lib:spawn(fun mock_connection/0)} end,
                        fun(Pid) ->
                                erlang:exit(Pid, kill),
                                ok
                        end),
    ok.

mock_connection() ->
    timer:sleep(10),
    mock_connection().

teardown(_) ->
    application:stop(gproc),
    application:stop(episcina),
    ok.

get_connections() ->
    {ok, C1} = episcina:get_connection(?POOL),
    {ok, C2} = episcina:get_connection(?POOL),
    ?assert(C1 =/= C2),
    ok = episcina:return_connection(?POOL, C1),
    ok = episcina:return_connection(?POOL, C2).

get_connection_timeout() ->
    {ok, C1} = episcina:get_connection(?POOL),
    {ok, C2} = episcina:get_connection(?POOL),
    {ok, C3} = episcina:get_connection(?POOL),
    {ok, C4} = episcina:get_connection(?POOL),
    {ok, C5} = episcina:get_connection(?POOL),
    ?assertMatch({error, timeout}, episcina:get_connection(?POOL, 100)),
    ok = episcina:return_connection(?POOL, C1),
    %% We should be able to get one now
    {ok, C6} = episcina:get_connection(?POOL),
    ok = episcina:return_connection(?POOL, C2),
    ok = episcina:return_connection(?POOL, C3),
    ok = episcina:return_connection(?POOL, C4),
    ok = episcina:return_connection(?POOL, C5),
    ok = episcina:return_connection(?POOL, C6).

return_twice() ->
    {ok, C} = episcina:get_connection(?POOL),
    ok = episcina:return_connection(?POOL, C),
    ok = episcina:return_connection(?POOL, C),
    {ok, C2} = episcina:get_connection(?POOL),
    ok = episcina:return_connection(?POOL, C2).

connection_dies() ->
    {ok, C1} = episcina:get_connection(?POOL),
    true = erlang:exit(C1, kill),
    {ok, C2} = episcina:get_connection(?POOL),
    ?assert(C1 =/= C2),
    ok = episcina:return_connection(?POOL, C2).

connection_owner_dies() ->
    Self = erlang:self(),
    Ref = erlang:make_ref(),
    proc_lib:spawn(fun() ->
                           {ok, C1} = episcina:get_connection(?POOL),
                           Self ! {Ref, connection, C1}
                   end),
    receive
        {Ref, connection, C1} ->
            {ok, C2} = episcina:get_connection(?POOL),
            ok = episcina:return_connection(?POOL, C2),
            ok = episcina:return_connection(?POOL, C1)
    end,
    {ok, C3} = episcina:get_connection(?POOL),
    ok = episcina:return_connection(?POOL, C3).
