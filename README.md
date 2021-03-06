HardCore
========

HardCore was created with the idea of creating an Erlang system with a
'hardened core'.

The problem: you have an Erlang system with multiple applications,
some of which may fail completely.  The system should continue to
function and respond to users even if some of those applications
crash.

One way to handle this problem is by means of a "circuit breaker" like
Fuse: https://github.com/jlouis/fuse

The Hardcore application takes another approach: rather than monitor a
single service, it monitors an entire application.

How it works
------------

Once started with `application:start(hardcore)`, you can start other
applications like so:

    hardcore:start(YourApp).

If you're working with a release, you'd want to start some
applications under HardCore management like so, in your sys.config file:


    {hardcore, [{apps, [YourApp,
                        {AnotherApp, {Module, Function, []}}]}]},

Callbacks
---------

When an application stops or starts, we need to be able to act on
that.  By default, the hardcore_backoff_server module handles restarts
with an exponential backoff strategy.  Using the callback argument to
hardcore:start, it's possible to pass in a tuple like

    {hardcore_backoff_server, state_change, []}

With a module, function, and optional arguments.  When the state
changes to one of `stopped` or `started`, the function is called like so:

    Module:Function(stopped | started, AppName, Args)

As in the example above, the callback can also be passed in as part of
the config file.
