%%%-------------------------------------------------------------------
%%% @hidden
%%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%%% @copyright (C) 2010 Fernando Benavides <fernando.benavides@inakanetworks.com>
%%% @doc apns4erl main supervisor
%%% @end
%%%-------------------------------------------------------------------
-module(apns_sup).
-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').

-behaviour(supervisor).

-include("apns.hrl").

-export([start_link/0, start_connection/1, start_connection/2]).
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================
%% @hidden
-spec start_link() -> {ok, pid()} | ignore | {error, {already_started, pid()} | shutdown | term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @hidden
-spec start_connection(#apns_connection{}) -> {ok, pid()} | {error, term()}.
start_connection(Connection) ->
  supervisor:start_child(?MODULE, [Connection]).

%% @hidden
-spec start_connection(atom(), #apns_connection{}) -> {ok, pid()} | {error, term()}.
start_connection(Name, Connection) ->
  supervisor:start_child(?MODULE, [Name, Connection]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
%% @hidden
-spec init(_) ->  {ok, {{simple_one_for_one, 5, 10}, [{connection, {apns_connection, start_link, []}, transient, 5000, worker, [apns_connection]}]}}.
init(_) ->
  {ok,
   {{simple_one_for_one, 5, 10},
    [{connection, {apns_connection, start_link, []},
      transient, 5000, worker, [apns_connection]}]}}.