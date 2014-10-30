HardCore
========

HardCore was created with the idea of creating an Erlang system with a
'hardened core'.

The problem: you have an Erlang system with multiple applications.
The system should continue to function and respond to users even if
some of those applications crash.

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


    {hardcore, [{apps, [YourApp]}]},

