%%%-------------------------------------------------------------------
%%% @author David N. Welton <davidw@dedasys.com>
%%% @copyright (C) 2014, David N. Welton
%%% @doc
%%%
%%% @end
%%% Created : 24 Sep 2014 by David N. Welton <davidw@dedasys.com>
%%%-------------------------------------------------------------------
-module(hardcore).

%% API
-export([start/1, start/2, stop/1, manage/1, running/0, managed/0]).

%%%===================================================================
%%% API
%%%===================================================================

start(AppName) ->
    start(AppName, {hardcore_backoff_server, state_change, []}).

start(AppName, Callback) ->
    gen_server:call(hardcore_server, {start, AppName, Callback}).

stop(AppName) ->
    gen_server:call(hardcore_server, {stop, AppName}).

manage(AppName) ->
    gen_server:call(hardcore_server, {manage, AppName}).

running() ->
    hardcore_server:running().

managed() ->
    hardcore_server:managed().

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
