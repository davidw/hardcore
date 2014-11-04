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
    %% Remove the error handler if it has been left sitting around:
    error_logger:delete_report_handler(hardcore_events),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Server = ?CHILD(hardcore_server, worker),
    DefaultCallback = ?CHILD(hardcore_backoff_server, worker),
    {ok, { {one_for_one, 100, 100}, [Server, DefaultCallback]} }.

