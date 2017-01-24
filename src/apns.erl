%%% @doc Main module for apns4erl API. Use this one from your own applications.
%%%
%%% Copyright 2017 Erlang Solutions Ltd.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%% @end
%%% @copyright Inaka <hello@inaka.net>
%%%
-module(apns).
-author("Felipe Ripoll <felipes@inakanetworks.com>").

%% API
-export([ start/0
        , stop/0
        , connect/1
        ]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Used when starting the application on the shell.
-spec start() -> ok.
start() ->
  {ok, _} = application:ensure_all_started(apns),
  ok.

%% @doc Stops the Application
-spec stop() -> ok.
stop() ->
  ok = application:stop(apns),
  ok.

%% @doc Connect to APNs service with Provider Certificate
-spec connect( apns_connection:name()
             | apns_connection:connection()) -> {ok, pid()}.
connect(ConnectionName) when is_atom(ConnectionName) ->
  DefaultConnection = apns_connection:default_connection(ConnectionName),
  connect(DefaultConnection);
connect(Connection) ->
  {ok, _} = apns_sup:create_connection(Connection),
  {ok, whereis(apns_connection:name(Connection))}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================
