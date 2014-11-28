%%% @hidden
-module(apns_app).
-author('Brujo Benavides <elbrujohalcon@inaka.net>').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
%% @hidden
-spec start(normal | {takeover, node()} | {failover, node()}, term()) ->
  {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    apns_sup:start_link().

%% @hidden
-spec stop([]) -> ok.
stop([]) -> ok.
