%%%-------------------------------------------------------------------
%%% @author David N. Welton <davidw@dedasys.com>
%%% @copyright (C) 2014, David N. Welton
%%% @doc
%%%
%%% @end
%%% Created : 24 Sep 2014 by David N. Welton <davidw@dedasys.com>
%%%-------------------------------------------------------------------
-module(hardcore_server).

-behaviour(gen_server).

%% API
-export([start_link/0, app_started/1, app_stopped/2, running/0, managed/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
          callbacks::list(),
          managed_apps::sets:set(),
          running_apps::sets:set()
         }).

%%%===================================================================
%%% API
%%%===================================================================

app_started(AppName) ->
    gen_server:cast(?SERVER, {app_started, AppName}).

app_stopped(AppName, Reason) ->
    gen_server:cast(?SERVER, {app_stopped, AppName, Reason}).

running() ->
    gen_server:call(?SERVER, running).

managed() ->
    gen_server:call(?SERVER, managed).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    gen_server:cast(?SERVER, add_handler),
    {ok, #state{callbacks = [],
                managed_apps = sets:new(),
                running_apps = sets:new()}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({start, AppName, Callback}, From, State) ->
    %% Don't actually start it: we first want to register it as
    %% something to listen in on, in case it crashes right away.
    gen_server:cast(?SERVER, {start_application, AppName, From}),
    NewCallbacks = proplists:delete(AppName, State#state.callbacks) ++
        [{AppName, Callback}],
    {noreply, add_app_managed(AppName, State#state{callbacks = NewCallbacks})};

%% Stop it and stop listening in on it.
handle_call({stop, AppName}, _From, State) ->
    Res = application:stop(AppName),
    NewState = apply_funs_to_state(AppName, State,
                                   [fun remove_app_managed/2,
                                    fun remove_app_running/2]),

    {reply, {Res, result_proplist(NewState)}, NewState};

%% In this case it's already running or we return an error.
handle_call({manage, AppName, Callback}, _From, State) ->
    case lists:keyfind(AppName, 1, application:which_applications()) of
        false ->
            {reply, {error, not_running, AppName}, State};
        _ ->
            NewCallbacks = proplists:delete(AppName, State#state.callbacks) ++
                [{AppName, Callback}],
            NewState = apply_funs_to_state(AppName, State,
                                           [fun add_app_running/2,
                                            fun add_app_managed/2]),
            {reply, result_proplist(State), NewState#state{callbacks = NewCallbacks}}
    end;

handle_call(running, _From, State) ->
    {reply, sets:to_list(State#state.running_apps), State};
handle_call(managed, _From, State) ->
    {reply, sets:to_list(State#state.managed_apps), State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_cast(add_handler, State) ->
    ok = error_logger:add_report_handler(hardcore_events),
    {noreply, State};

handle_cast({start_application, AppName, From}, State) ->
    case application:ensure_all_started(AppName, temporary) of
        {ok, _Started} ->
            gen_server:reply(From, {ok, result_proplist(State)}),
            {noreply, State};
        Err ->
            gen_server:reply(From, {Err, result_proplist(State)}),
            {noreply, remove_app_running(AppName, State)}
    end;

%% This comes from hardcore_events when it notices that the
%% application has started.
handle_cast({app_started, AppName}, State) ->
    case sets:is_element(AppName, State#state.managed_apps) of
        true ->
            {M, F, A} = proplists:get_value(AppName, State#state.callbacks),
            catch(M:F(started, AppName, A)),
            {noreply, add_app_running(AppName, State)};
        _ ->
            {noreply, State}
    end;

%% This comes from hardcore_events when it notices that the
%% application has stopped.
handle_cast({app_stopped, AppName, _Reason}, State) ->
    case sets:is_element(AppName, State#state.managed_apps) of
        true ->
            {M, F, A} = proplists:get_value(AppName, State#state.callbacks),
            catch(M:F(stopped, AppName, A)),
            {noreply, remove_app_running(AppName, State)};
        _ ->
            {noreply, State}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    error_logger:delete_report_handler(?SERVER),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

remove_app_running(AppName, State) ->
    NewApps = sets:del_element(AppName, State#state.running_apps),
    State#state{running_apps = NewApps}.

remove_app_managed(AppName, State) ->
    NewApps = sets:del_element(AppName, State#state.managed_apps),
    State#state{managed_apps = NewApps}.

add_app_managed(AppName, State) ->
    NewApps = sets:add_element(AppName, State#state.managed_apps),
    State#state{managed_apps = NewApps}.

add_app_running(AppName, State) ->
    NewApps = sets:add_element(AppName, State#state.running_apps),
    State#state{running_apps = NewApps}.

result_proplist(State) ->
    [{managed, sets:to_list(State#state.managed_apps)},
     {running, sets:to_list(State#state.running_apps)}].

apply_funs_to_state(AppName, State, Funs) ->
    lists:foldl(fun(F, S) -> F(AppName, S) end, State, Funs).
