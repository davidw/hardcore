%%%-------------------------------------------------------------------
%%% @author David N. Welton <davidw@dedasys.com>
%%% @copyright (C) 2014, David N. Welton
%%% @doc
%%%
%%% @end
%%% Created : 31 Oct 2014 by David N. Welton <davidw@dedasys.com>
%%%-------------------------------------------------------------------
-module(hardcore_backoff_server).

-behaviour(gen_server).

%% API
-export([start_link/0, state_change/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(MAX_RESTART_SECONDS, 100).

-record(state, {restart_waits::list(),
                last_stop_times::list()}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

state_change(NewState, AppName, _Args) ->
    gen_server:cast(?SERVER, {state_change, NewState, AppName}).
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
    {ok, #state{restart_waits = [], last_stop_times = []}}.

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
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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

handle_cast({restart, AppName}, State) ->
    hardcore:start(AppName),
    {noreply, State};

handle_cast({state_change, started, AppName}, State) ->
    %% The application has started.  Remove any value associated with
    %% it in the wait time list.

    LastStop = proplists:get_value(AppName, State#state.last_stop_times, {0, 0, 0}),
    LastWait = proplists:get_value(AppName, State#state.restart_waits, 0),

    case (timer:now_diff(erlang:now(), LastStop) / 1000000) > LastWait of
        true ->
            NewRestartWaits = proplists:delete(AppName,
                                               State#state.restart_waits),
            NewStopTimes = proplists:delete(AppName,
                                            State#state.last_stop_times);
        _ ->
            NewRestartWaits = State#state.restart_waits,
            NewStopTimes = State#state.last_stop_times
    end,

    {noreply,
     State#state{restart_waits = NewRestartWaits,
                 last_stop_times = NewStopTimes}};

handle_cast({state_change, stopped, AppName}, State) ->
    case proplists:get_value(AppName, State#state.restart_waits) of
        undefined ->
            RestartIn = 1,
            RestartTime = 1;
        RestartTime ->
            RestartIn = lists:min([RestartTime * 2, ?MAX_RESTART_SECONDS])
    end,
    lager:info("Application stopped: ~p - restarting in ~p seconds", [AppName, RestartIn]),
    {ok, _Tref} =
        timer:apply_after(RestartTime * 1000,
                          gen_server, cast,
                          [?SERVER, {restart, AppName}]),
    NewRestartWaits =
        proplists:delete(AppName, State#state.restart_waits) ++
        [{AppName, RestartIn}],
    NewStopTimes =
        proplists:delete(AppName, State#state.last_stop_times) ++
        [{AppName, erlang:now()}],
    {noreply, State#state{restart_waits = NewRestartWaits, last_stop_times = NewStopTimes}}.

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
