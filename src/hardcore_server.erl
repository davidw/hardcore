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
          managed_apps,
          running_apps
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
    {ok, #state{managed_apps = sets:new(),
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
handle_call({start, AppName}, From, State) ->
    %% Don't actually start it, we first want to register it as
    %% something to listen in on, in case it crashes right away.
    gen_server:cast(?SERVER, {start_application, AppName, From}),
    {noreply, add_app_managed(AppName, State)};

handle_call({stop, AppName}, _From, State) ->
    Res = application:stop(AppName),
    NewState = remove_app_managed(AppName, remove_app_running(AppName, State)),
    {reply, {Res, result_proplist(NewState)}, NewState};

%% In this case it's already running or we return an error.
handle_call({manage, AppName}, _From, State) ->
    case lists:keyfind(AppName, 1, application:which_applications()) of
        false ->
            {reply, {error, not_running, AppName}, State};
        _ ->
            NewState = add_app_running(AppName, add_app_managed(AppName, State)),
            {reply, result_proplist(State), NewState}
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

handle_cast({start_application, AppName, From}, State) ->
    case application:ensure_all_started(AppName, temporary) of
        ok ->
            gen_server:reply(From, {ok, result_proplist(State)}),
            {noreply, State};
        Err ->
            gen_server:reply(From, {Err, result_proplist(State)}),
            {noreply, remove_app_running(AppName, State)}
    end;

handle_cast({app_started, AppName}, State) ->
    case sets:is_element(AppName, State#state.managed_apps) of
        true ->
            {noreply, add_app_running(AppName, State)};
        _ ->
            {noreply, State}
    end;

handle_cast({app_stopped, AppName, _Reason}, State) ->
    case sets:is_element(AppName, State#state.managed_apps) of
        true ->
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
