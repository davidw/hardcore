-module(hardcore_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Res = hardcore_sup:start_link(),
    {ok, StartApps} = application:get_env(hardcore, apps),
    lists:map(fun({App, Callback}) ->
                      hardcore:start(App, Callback);
                 (App) ->
                      hardcore:start(App)
              end, StartApps),
    Res.

stop(_State) ->
    ok.
