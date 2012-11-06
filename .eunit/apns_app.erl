%%%-------------------------------------------------------------------
%%% @hidden
%%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%%% @copyright (C) 2010 Fernando Benavides <fernando.benavides@inakanetworks.com>
%%% @doc Apple Push Notification Server for Erlang
%%% @end
%%%-------------------------------------------------------------------
-module(apns_app).
-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
%% @hidden
-spec start(normal | {takeover, node()} | {failover, node()}, term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    apns_sup:start_link().

%% @hidden
-spec stop([]) -> ok.
stop([]) -> ok.