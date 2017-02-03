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
-author("Felipe Ripoll <felipe@inakanetworks.com>").

%% API
-export([ start/0
        , stop/0
        , connect/2
        , close_connection/1
        , push_notification/3
        , push_notification/4
        , push_notification_token/4
        , push_notification_token/5
        , default_headers/0
        , generate_token/2
        ]).

-export_type([ json/0
             , device_id/0
             , response/0
             , token/0
             , headers/0
             ]).

-type json()      :: #{binary() => binary() | json()}.
-type device_id() :: binary().
-type response()  :: { integer()          % HTTP2 Code
                     , [term()]           % Response Headers
                     , [term()] | no_body % Response Body
                     } | timeout.
-type token()     :: binary().
-type headers()   :: #{ apns_id          => binary()
                      , apns_expiration  => binary()
                      , apns_priority    => binary()
                      , apns_topic       => binary()
                      , apns_collapse_id => binary()
                      , apns_auth_token  => binary()
                      }.

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

%% @doc Connects to APNs service with Provider Certificate
-spec connect( apns_connection:type(), apns_connection:name()) ->
  {ok, pid()} | {error, timeout}.
connect(Type, ConnectionName) ->
  DefaultConnection = apns_connection:default_connection(Type, ConnectionName),
  connect(DefaultConnection).

%% @doc Closes the connection with APNs service.
-spec close_connection(apns_connection:name()) -> ok.
close_connection(ConnectionName) ->
  apns_connection:close_connection(ConnectionName).

%% @doc Push notification to APNs. It will use the headers provided on the
%%      environment variables.
-spec push_notification( apns_connection:name()
                       , device_id()
                       , json()
                       ) -> response().
push_notification(ConnectionName, DeviceId, JSONMap) ->
  Headers = default_headers(),
  push_notification(ConnectionName, DeviceId, JSONMap, Headers).

%% @doc Push notification to certificate APNs Connection.
-spec push_notification( apns_connection:name()
                       , device_id()
                       , json()
                       , headers()
                       ) -> response().
push_notification(ConnectionName, DeviceId, JSONMap, Headers) ->
  Notification = jsx:encode(JSONMap),
  apns_connection:push_notification( ConnectionName
                                   , DeviceId
                                   , Notification
                                   , Headers
                                   ).

%% @doc Push notification to APNs with authentication token. It will use the
%%      headers provided on the environment variables.
-spec push_notification_token( apns_connection:name()
                             , token()
                             , device_id()
                             , json()
                             ) -> response().
push_notification_token(ConnectionName, Token, DeviceId, JSONMap) ->
  Headers = default_headers(),
  push_notification_token(ConnectionName, Token, DeviceId, JSONMap, Headers).

%% @doc Push notification to authentication token APNs Connection.
-spec push_notification_token( apns_connection:name()
                       , token()
                       , device_id()
                       , json()
                       , headers()
                       ) -> response().
push_notification_token(ConnectionName, Token, DeviceId, JSONMap, Headers) ->
  Notification = jsx:encode(JSONMap),
  apns_connection:push_notification( ConnectionName
                                   , Token
                                   , DeviceId
                                   , Notification
                                   , Headers
                                   ).

-spec generate_token(binary(), binary()) -> token().
generate_token(TeamId, KeyId) ->
  Algorithm = <<"ES256">>,
  Header = jsx:encode([ {alg, Algorithm}
                      , {typ, <<"JWT">>}
                      , {kid, KeyId}
                      ]),
  Payload = jsx:encode([ {iss, TeamId}
                       , {iat, epoch()}
                       ]),
  HeaderEncoded = base64url:encode(Header),
  PayloadEncoded = base64url:encode(Payload),
  DataEncoded = <<HeaderEncoded/binary, $., PayloadEncoded/binary>>,
  Signature = sign(DataEncoded),
  <<DataEncoded/binary, $., Signature/binary>>.

%% @doc Get the default headers from environment variables.
-spec default_headers() -> apns:headers().
default_headers() ->
  Headers = [ apns_id
            , apns_expiration
            , apns_priority
            , apns_topic
            , apns_collapse_id
            ],

  default_headers(Headers, #{}).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% Connects to APNs service
-spec connect(apns_connection:connection()) -> {ok, pid()} | {error, timeout}.
connect(Connection) ->
  {ok, _} = apns_sup:create_connection(Connection),
  Server = whereis(apns_connection:name(Connection)),
  apns_connection:wait_apns_connection_up(Server).

%% Build a headers() structure from environment variables.
-spec default_headers(list(), headers()) -> headers().
default_headers([], Headers) ->
  Headers;
default_headers([Key | Keys], Headers) ->
  case application:get_env(apns, Key) of
    {ok, undefined} ->
      default_headers(Keys, Headers);
    {ok, Value} ->
      NewHeaders = Headers#{Key => to_binary(Value)},
      default_headers(Keys, NewHeaders)
  end.

%% Convert to binary
to_binary(Value) when is_integer(Value) ->
  list_to_binary(integer_to_list(Value));
to_binary(Value) when is_list(Value) ->
  list_to_binary(Value);
to_binary(Value) when is_binary(Value) ->
  Value.

-spec sign(binary()) -> binary().
sign(Data) ->
  {ok, KeyPath} = application:get_env(apns, token_keyfile),
  Command = "printf '" ++
            binary_to_list(Data) ++
            "' | openssl dgst -binary -sha256 -sign " ++ KeyPath ++ " | base64",
  {0, Result} = ktn_os:command(Command),
  list_to_binary(Result).

-spec epoch() -> integer().
epoch() ->
  {M, S, _} = os:timestamp(),
  M * 1000000 + S.
