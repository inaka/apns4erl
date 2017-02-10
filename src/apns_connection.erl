%%% @doc This gen_server handles the APNs Connection.
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
-module(apns_connection).
-author("Felipe Ripoll <felipe@inakanetworks.com>").

-behaviour(gen_server).

%% API
-export([ start_link/2
        , default_connection/2
        , name/1
        , host/1
        , port/1
        , certfile/1
        , keyfile/1
        , type/1
        , gun_connection/1
        , close_connection/1
        , push_notification/4
        , push_notification/5
        , wait_apns_connection_up/1
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-export_type([ name/0
             , host/0
             , port/0
             , path/0
             , connection/0
             , notification/0
             , type/0
             ]).

-type name()         :: atom().
-type host()         :: string() | inet:ip_address().
-type path()         :: string().
-type notification() :: binary().
-type type()         :: cert | token.
-opaque connection() :: #{ name       := name()
                         , apple_host := host()
                         , apple_port := inet:port_number()
                         , certfile   => path()
                         , keyfile    => path()
                         , type       := type()
                         }.

-type state()        :: #{ connection      := connection()
                         , gun_connection  := pid()
                         , client          := pid()
                         , backoff         := non_neg_integer()
                         , backoff_ceiling := non_neg_integer()
                         }.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc starts the gen_server
-spec start_link(connection(), pid()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(Connection, Client) ->
  Name = name(Connection),
  gen_server:start_link({local, Name}, ?MODULE, {Connection, Client}, []).

%% @doc Builds a connection() map from the environment variables.
-spec default_connection(type(), name()) -> connection().
default_connection(cert, ConnectionName) ->
  {ok, Host} = application:get_env(apns, apple_host),
  {ok, Port} = application:get_env(apns, apple_port),
  {ok, Certfile} = application:get_env(apns, certfile),
  {ok, Keyfile} = application:get_env(apns, keyfile),

  #{ name       => ConnectionName
   , apple_host => Host
   , apple_port => Port
   , certfile   => Certfile
   , keyfile    => Keyfile
   , type       => cert
  };
default_connection(token, ConnectionName) ->
  {ok, Host} = application:get_env(apns, apple_host),
  {ok, Port} = application:get_env(apns, apple_port),

  #{ name       => ConnectionName
   , apple_host => Host
   , apple_port => Port
   , type       => token
  }.

%% @doc Close the connection with APNs gracefully
-spec close_connection(name()) -> ok.
close_connection(ConnectionName) ->
  gen_server:cast(ConnectionName, stop).

%% @doc Returns the gun's connection PID. This function is only used in tests.
-spec gun_connection(name()) -> pid().
gun_connection(ConnectionName) ->
  gen_server:call(ConnectionName, gun_connection).

%% @doc Pushes notification to certificate APNs connection.
-spec push_notification( name()
                       , apns:device_id()
                       , notification()
                       , apns:headers()) -> apns:response().
push_notification(ConnectionName, DeviceId, Notification, Headers) ->
  gen_server:call(ConnectionName, { push_notification
                                  , DeviceId
                                  , Notification
                                  , Headers
                                  }).

%% @doc Pushes notification to certificate APNs connection.
-spec push_notification( name()
                       , apns:token()
                       , apns:device_id()
                       , notification()
                       , apns:headers()) -> apns:response().
push_notification(ConnectionName, Token, DeviceId, Notification, Headers) ->
  gen_server:call(ConnectionName, { push_notification
                                  , Token
                                  , DeviceId
                                  , Notification
                                  , Headers
                                  }).

%% @doc Waits until receive the `connection_up` message
-spec wait_apns_connection_up(pid()) -> {ok, pid()} | {error, timeout}.
wait_apns_connection_up(Server) ->
  receive
    {connection_up, Server} -> {ok, Server};
    {timeout, Server}       -> {error, timeout}
  end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init({connection(), pid()}) -> {ok, State :: state(), timeout()}.
init({Connection, Client}) ->
  GunConnectionPid = open_gun_connection(Connection),

  {ok, #{ connection      => Connection
        , gun_connection  => GunConnectionPid
        , client          => Client
        , backoff         => 1
        , backoff_ceiling => application:get_env(apns, backoff_ceiling, 10)
        }, 0}.

-spec handle_call( Request :: term(), From :: {pid(), term()}, State) ->
  {reply, ok, State}.
handle_call(gun_connection, _From, #{gun_connection := GunConn} = State) ->
  {reply, GunConn, State};
handle_call( {push_notification, DeviceId, Notification, Headers}
           , _From
           , State) ->
  #{gun_connection := GunConn} = State,
  Response = push(GunConn, DeviceId, Headers, Notification),
  {reply, Response, State};
handle_call( {push_notification, Token, DeviceId, Notification, HeadersMap}
           , _From
           , State) ->
  #{gun_connection := GunConn} = State,
  Headers = add_authorization_header(HeadersMap, Token),
  Response = push(GunConn, DeviceId, Headers, Notification),
  {reply, Response, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

-spec handle_cast(Request :: term(), State) ->
  {noreply, State}.
handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast(_Request, State) ->
  {noreply, State}.

-spec handle_info(Info :: timeout() | term(), State) -> {noreply, State}.
handle_info( {gun_down, GunConn, http2, closed, _, _}
           , #{ gun_connection  := GunConn
              , client          := Client
              , backoff         := Backoff
              , backoff_ceiling := Ceiling
              } = State) ->
  ok = gun:close(GunConn),
  Client ! {reconnecting, self()},
  Sleep = backoff(Backoff, Ceiling) * 1000, % seconds to wait before reconnect
  {ok, _} = timer:send_after(Sleep, reconnect),
  {noreply, State#{backoff => Backoff + 1}};
handle_info(reconnect, State) ->
  #{ connection      := Connection
   , client          := Client
   , backoff         := Backoff
   , backoff_ceiling := Ceiling
   } = State,
  GunConn = open_gun_connection(Connection),
  {ok, Timeout} = application:get_env(apns, timeout),
  case gun:await_up(GunConn, Timeout) of
    {ok, http2} ->
      Client ! {connection_up, self()},
      {noreply, State#{ gun_connection => GunConn
                      , backoff        => 1}};
    {error, timeout} ->
      ok = gun:close(GunConn),
      Sleep = backoff(Backoff, Ceiling) * 1000, % seconds to wait
      {ok, _} = timer:send_after(Sleep, reconnect),
      {noreply, State#{backoff => Backoff + 1}}
  end;
handle_info(timeout, #{gun_connection := GunConn, client := Client} = State) ->
  {ok, Timeout} = application:get_env(apns, timeout),
  case gun:await_up(GunConn, Timeout) of
    {ok, http2} ->
      Client ! {connection_up, self()},
      {noreply, State};
    {error, timeout} ->
      Client ! {timeout, self()},
      {stop, timeout, State}
  end;
handle_info(_Info, State) ->
  {noreply, State}.

-spec terminate( Reason :: (normal | shutdown | {shutdown, term()} | term())
               , State  :: state()
               ) -> ok.
terminate(_Reason, _State) ->
  ok.

-spec code_change(OldVsn :: term() | {down, term()}
                 , State
                 , Extra :: term()
                 ) -> {ok, State}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Connection getters/setters Functions
%%%===================================================================

-spec name(connection()) -> name().
name(#{name := ConnectionName}) ->
  ConnectionName.

-spec host(connection()) -> host().
host(#{apple_host := Host}) ->
  Host.

-spec port(connection()) -> inet:port_number().
port(#{apple_port := Port}) ->
  Port.

-spec certfile(connection()) -> path().
certfile(#{certfile := Certfile}) ->
  Certfile.

-spec keyfile(connection()) -> path().
keyfile(#{keyfile := Keyfile}) ->
  Keyfile.

-spec type(connection()) -> type().
type(#{type := Type}) ->
  Type.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

-spec open_gun_connection(connection()) -> GunConnectionPid :: pid().
open_gun_connection(Connection) ->
  Host = host(Connection),
  Port = port(Connection),

  TransportOpts = case type(Connection) of
    cert ->
      Certfile = certfile(Connection),
      Keyfile = keyfile(Connection),
      [{certfile, Certfile}, {keyfile, Keyfile}];
    token ->
      []
  end,

  {ok, GunConnectionPid} = gun:open( Host
                                   , Port
                                   , #{ protocols => [http2]
                                      , transport_opts => TransportOpts
                                      }),
  GunConnectionPid.

-spec get_headers(apns:headers()) -> list().
get_headers(Headers) ->
  List = [ {<<"apns-id">>, apns_id}
         , {<<"apns-expiration">>, apns_expiration}
         , {<<"apns-priority">>, apns_priority}
         , {<<"apns-topic">>, apns_topic}
         , {<<"apns-collapse_id">>, apns_collapse_id}
         , {<<"authorization">>, apns_auth_token}
         ],
  F = fun({ActualHeader, Key}) ->
    case (catch maps:get(Key, Headers)) of
      {'EXIT', {{badkey, Key}, _}} -> [];
      Value -> [{ActualHeader, Value}]
    end
  end,
  lists:flatmap(F, List).

-spec get_device_path(apns:device_id()) -> binary().
get_device_path(DeviceId) ->
  <<"/3/device/", DeviceId/binary>>.

-spec add_authorization_header(apns:headers(), apnd:token()) -> apns:headers().
add_authorization_header(Headers, Token) ->
  Headers#{apns_auth_token => <<"bearer ", Token/binary>>}.

-spec push(pid(), apns:device_id(), apns:headers(), notification()) ->
  apns:response().
push(GunConn, DeviceId, HeadersMap, Notification) ->
  {ok, Timeout} = application:get_env(apns, timeout),
  Headers = get_headers(HeadersMap),
  Path = get_device_path(DeviceId),
  StreamRef = gun:post(GunConn, Path, Headers, Notification),
  case gun:await(GunConn, StreamRef, Timeout) of
    {response, fin, Status, ResponseHeaders} ->
      {Status, ResponseHeaders, no_body};
    {response, nofin, Status, ResponseHeaders} ->
      {ok, Body} = gun:await_body(GunConn, StreamRef, Timeout),
      DecodedBody = jsx:decode(Body),
      {Status, ResponseHeaders, DecodedBody};
    {error, timeout} -> timeout
  end.

-spec backoff(non_neg_integer(), non_neg_integer()) -> non_neg_integer().
backoff(N, Ceiling) ->
  case (math:pow(2, N) - 1) of
    R when R > Ceiling ->
      Ceiling;
    NextN ->
      NString = float_to_list(NextN, [{decimals, 0}]),
      list_to_integer(NString)
  end.
