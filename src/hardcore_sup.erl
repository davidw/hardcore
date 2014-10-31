-module(hardcore_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Events = ?CHILD(hardcore_events, worker),
    Server = ?CHILD(hardcore_server, worker),
    DefaultCallback = ?CHILD(hardcore_backoff_server, worker),
    {ok, { {one_for_one, 100, 100}, [Server, Events, DefaultCallback]} }.

