EPiscina - A Simple Erlang Connection Pool
==========================================

episcina is designed as a pool for resources. Generally those
resources are expected to be connections to some type of external
thing like a sql system. However, there is nothing sql specific about
EPiscina. You may generally consider it a small very focused pool
implementation that does not try to take ownership of your work.

How It Works
------------
episcina will create any pools defined in the episcina's 'pools'
environment parameter, which is a proplist that tells episcina how to
connect. There are several parameters that are required.

* `size` - The maximum size of the pool
* `timeout` - The maximum number of milliseconds that the a caller is
  allowed to hold a connection. See below for the consequences of
  holding a connection open too long.
* `connect_provider` - This is the function + arguments that will be
  used to provide connections. It contains the name of the module, the
  name of the function and the arguments to pass to that
  function. There is an example below.
* `close_provider` - This is tthe function + arguments that will be
  used to close connections. It contains the name of the module, the
  name of the function and any arguments needed to be passed to that
  function. There is an example below.

#### sys.config file example:

    {episcina, [{pools, [{db1,
                          [{size, 10},
                           {timeout, 10000},
                           {connect_provider, {pgsql, connect,
                                               ["localhost",
                                                5432,
                                                "my supersecret pass",
                                                "postgresql",
                                                [{database, "foobar"}]]}},
                           {close_provider, {pgsql, close, []}}]}]}]}.



Pool Usage
----------

    {ok, C} = episcina:get_connection(Pool, Timeout).

* `Pool` - Name of pool.
* `Timeout` - Time, in milliseconds, to wait for a free connection.

    ok = episcina:return_connection(Pool, Connection).

Details
-------

* episcina monitors the process which called get_connection and returns the
allocated connection to the pool if that process dies.
* If a connection dies, a new one is created and added to the pool in
its place.
* If the caller holds the pool longer the specified pool timeout then
  an exit message is sent to the calling process and the connection is
  returned to the pool.
