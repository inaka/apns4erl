%%% @doc This gen_statem handles the APNs Connection.
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

-behaviour(gen_statem).

-include_lib("public_key/include/public_key.hrl").
-include("apns.hrl").

%% API
-export([ start_link/2
        , default_connection/2
        , name/1
        , host/1
        , port/1
        , certdata/1
        , certfile/1
        , keydata/1
        , keyfile/1
        , type/1
        , gun_pid/1
        , close_connection/1
        , push_notification/4
        , wait_apns_connection_up/1
        ]).

%% gen_statem callbacks
-export([ init/1
        , callback_mode/0
        , open_connection/3
        , open_origin/3
        , open_proxy/3
        , open_common/3
        , await_up/3
        , proxy_connect_to_origin/3
        , await_tunnel_up/3
        , connected/3
        , down/3
        , code_change/4
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
-type type()         :: certdata | cert | token.
-type keydata()      :: {'RSAPrivateKey' | 'DSAPrivateKey' | 'ECPrivateKey' |
                         'PrivateKeyInfo'
                        , binary()}.
-type proxy_info()   :: #{ type       := connect
                         , host       := host()
                         , port       := inet:port_number()
                         , username   => iodata()
                         , password   => iodata()
                         }.
-type connection()   :: #{ name       := name()
                         , apple_host := host()
                         , apple_port := inet:port_number()
                         , certdata   => binary()
                         , certfile   => path()
                         , keydata    => keydata()
                         , keyfile    => path()
                         , timeout    => integer()
                         , type       := type()
                         , proxy_info => proxy_info()
                         }.

-type state()        :: #{ connection      := connection()
                         , gun_pid         => pid()
                         , gun_monitor     => reference()
                         , gun_connect_ref => reference()
                         , client          := pid()
                         , backoff         := non_neg_integer()
                         , backoff_ceiling := non_neg_integer()
                         , queue           => list()
                         }.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc starts the gen_statem
-spec start_link(connection(), pid()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(#{name := undefined} = Connection, Client) ->
  gen_statem:start_link(?MODULE, {Connection, Client}, []);
start_link(Connection, Client) ->
  Name = name(Connection),
  gen_statem:start_link({local, Name}, ?MODULE, {Connection, Client}, []).

%% @doc Builds a connection() map from the environment variables.
-spec default_connection(type(), name()) -> connection().
default_connection(certdata, ConnectionName) ->
  Env = application:get_env(apns, env, development),
  Port = application:get_env(apns, apple_port, 443),
  Timeout = application:get_env(apns, timeout, 5000),
  FeedBack = application:get_env(apns, feedback, undefined),

  {ok, Cert} = application:get_env(apns, certdata),
  {ok, Key} = application:get_env(apns, keydata),

  #{ name       => ConnectionName
   , apple_host => host(Env)
   , apple_port => Port
   , certdata   => Cert
   , keydata    => Key
   , timeout    => Timeout
   , feedback   => FeedBack
   , type       => certdata
  };
default_connection(cert, ConnectionName) ->
  Env = application:get_env(apns, env, development),
  Port = application:get_env(apns, apple_port, 443),
  Timeout = application:get_env(apns, timeout, 5000),
  FeedBack = application:get_env(apns, feedback, undefined),

  {ok, Certfile} = application:get_env(apns, certfile),
  {ok, Keyfile} = application:get_env(apns, keyfile),

  #{ name       => ConnectionName
   , apple_host => host(Env)
   , apple_port => Port
   , certfile   => Certfile
   , keyfile    => Keyfile
   , timeout    => Timeout
   , feedback   => FeedBack
   , type       => cert
  };
default_connection(token, ConnectionName) ->
  Env = application:get_env(apns, env, development),
  Port = application:get_env(apns, apple_port, 443),
  Timeout = application:get_env(apns, timeout, 5000),
  FeedBack = application:get_env(apns, feedback, undefined),

  {ok, PrivKey} = application:get_env(apns, token_keyfile),
  {ok, TokenID} = application:get_env(apns, token_kid),
  {ok, TeamID} = application:get_env(apns, team_id),
  
  #{ name       => ConnectionName
   , apple_host => host(Env)
   , apple_port => Port
   , token_kid  => list_to_binary(TokenID)
   , team_id    => list_to_binary(TeamID)
   , token_file => PrivKey
   , jwt_token  => <<"">>
   , jwt_iat    => 0
   , timeout    => Timeout
   , feedback   => FeedBack
   , type       => token
  }.

verify_token(#{jwt_token := <<"">>} = Connection) ->
  update_token(Connection);

verify_token(#{jwt_iat := Iat} = Connection) ->
  Now = apns_utils:epoch(),
  if (Now - Iat - 3500) > 0 -> update_token(Connection);
     true -> Connection
  end.

update_token(#{token_kid := KeyId,
               team_id := TeamId,
               token_file := PrivKey} = Connection) ->
  Iat = apns_utils:epoch(),
  Token = generate_token(KeyId, TeamId, PrivKey, Iat),
  Connection#{jwt_token => Token, jwt_iat => Iat}.

generate_token(KeyId, TeamId, PrivKey, Iat) ->
  Algorithm = <<"ES256">>,
  Header = jsx:encode([ {alg, Algorithm}
                      , {typ, <<"JWT">>}
                      , {kid, KeyId}
                      ]),
  Payload = jsx:encode([ {iss, TeamId}
                       , {iat, Iat}
                       ]),

  ?DEBUG("generate_token ~p~n", [{KeyId, TeamId, PrivKey, Iat}]),
  HeaderEncoded = base64url:encode(Header),
  PayloadEncoded = base64url:encode(Payload),
  DataEncoded = <<HeaderEncoded/binary, $., PayloadEncoded/binary>>,
  ?DEBUG("generate_token encoded ~p~n", [DataEncoded]),
  
  {ok, Key} = file:read_file(PrivKey),
  [ECPrivateKeyPem] = public_key:pem_decode(Key),
  #'PrivateKeyInfo'{privateKey = ECPrivateKey} = public_key:pem_entry_decode(ECPrivateKeyPem),
  ECKey = public_key:der_decode('ECPrivateKey', ECPrivateKey),
  Encoded = public_key:sign(DataEncoded, sha256, ECKey),
  Signature = base64:encode(Encoded),
%%  Signature = apns_utils:strip_b64(BS),

  <<DataEncoded/binary, $., Signature/binary>>.

%% @doc Close the connection with APNs gracefully
-spec close_connection(name() | pid()) -> ok.
close_connection(ConnectionId) ->
  gen_statem:cast(ConnectionId, stop).

%% @doc Returns the gun's connection PID. This function is only used in tests.
-spec gun_pid(name() | pid()) -> pid().
gun_pid(ConnectionId) ->
  gen_statem:call(ConnectionId, gun_pid).

%% @doc Pushes notification to certificate APNs connection.
-spec push_notification( name() | pid()
                       , apns:device_id()
                       , notification()
                       , apns:headers()) -> apns:response() | {error, not_connection_owner}.
push_notification(ConnectionId, DeviceId, Notification, Headers) ->
  gen_statem:cast(ConnectionId, {push_notification, DeviceId, Notification, Headers}).

%% @doc Waits until the APNS connection is up.
%%
%% Note that this function does not need to be called before
%% sending push notifications, since they will be queued up
%% and sent when the connection is established.
-spec wait_apns_connection_up(pid()) -> ok.
wait_apns_connection_up(Server) ->
  gen_statem:call(Server, wait_apns_connection_up, infinity).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

-spec callback_mode() -> state_functions.
callback_mode() -> state_functions.

-spec init({connection(), pid()})
  -> { ok
     , open_connection
     , State :: state()
     , {next_event, internal, init}
     }.
init({Connection, Client}) ->
  quickrand:seed(),
  StateData = #{ connection      => Connection
               , client          => Client
               , backoff         => 1
               , backoff_ceiling => application:get_env(apns, backoff_ceiling, 10)
               , queue           => []
               },
  {ok, open_connection, StateData,
    {next_event, internal, init}}.

-spec open_connection(_, _, _) -> _.
open_connection(internal, _, #{connection := Connection} = StateData) ->
  NextState = case proxy(Connection) of
    #{type := connect} -> open_proxy;
    undefined          -> open_origin
  end,
  {next_state, NextState, StateData,
    {next_event, internal, init}}.

-spec open_origin(_, _, _) -> _.
open_origin(internal, _, #{connection := Connection} = StateData) ->
  Host = host(Connection),
  Port = port(Connection),
  TransportOpts = transport_opts(Connection),
  {next_state, open_common, StateData,
    {next_event, internal, { Host
                           , Port
                           , #{ protocols      => [http2]
                              , transport_opts => TransportOpts
                              , retry          => 0
                              }}}}.

-spec open_proxy(_, _, _) -> _.
open_proxy(internal, _, StateData) ->
  #{connection := Connection} = StateData,
  #{type := connect, host := ProxyHost, port := ProxyPort} = proxy(Connection),
  {next_state, open_common, StateData,
    {next_event, internal, { ProxyHost
                           , ProxyPort
                           , #{ protocols => [http]
                              , transport => tcp
                              , retry     => 0
                              }}}}.

%% This function exists only to make Elvis happy.
%% I do not think it makes things any easier to read.
-spec open_common(_, _, _) -> _.
open_common(internal, {Host, Port, Opts}, StateData) ->
  ?DEBUG("open_common ~p~n", [{Host, Port, Opts}]),
  {ok, GunPid} = gun:open(Host, Port, Opts),
  GunMon = monitor(process, GunPid),
  {next_state, await_up,
    StateData#{gun_pid => GunPid, gun_monitor => GunMon},
    {state_timeout, 15000, open_timeout}}.

-spec await_up(_, _, _) -> _.
await_up(info, {gun_up, GunPid, Protocol}, #{gun_pid := GunPid} = StateData) ->
  #{connection := Connection} = StateData,
  NextState = case proxy(Connection) of
    #{type := connect} when Protocol =:= http -> proxy_connect_to_origin;
    undefined when Protocol =:= http2 -> connected
  end,
  {next_state, NextState, StateData,
    {next_event, internal, on_connect}};
await_up(EventType, EventContent, StateData) ->
  handle_common(EventType, EventContent, ?FUNCTION_NAME, StateData, postpone).

-spec proxy_connect_to_origin(_, _, _) -> _.
proxy_connect_to_origin(internal, on_connect, StateData) ->
  #{connection := Connection, gun_pid := GunPid} = StateData,
  Host = host(Connection),
  Port = port(Connection),
  TransportOpts = transport_opts(Connection),
  Destination0 = #{ host => Host
                  , port => Port
                  , protocol => http2
                  , transport => tls
                  , tls_opts => TransportOpts
                  },
  Destination = case proxy(Connection) of
    #{ username := Username, password := Password } ->
      Destination0#{ username => Username, password => Password };
    _ ->
      Destination0
  end,
  ConnectRef = gun:connect(GunPid, Destination),
  {next_state, await_tunnel_up, StateData#{gun_connect_ref => ConnectRef},
    {state_timeout, 30000, proxy_connect_timeout}}.

-spec await_tunnel_up(_, _, _) -> _.
await_tunnel_up( info
               , {gun_response, GunPid, ConnectRef, fin, 200, _}
               , #{gun_pid := GunPid, gun_connect_ref := ConnectRef} = StateData) ->
  {next_state, connected, StateData,
    {next_event, internal, on_connect}};
await_tunnel_up(EventType, EventContent, StateData) ->
  handle_common(EventType, EventContent, ?FUNCTION_NAME, StateData, postpone).

-spec connected(_, _, _) -> _.
connected(internal, on_connect, #{client := Client}) ->
  Client ! {connection_up, self()},
  keep_state_and_data;
connected({call, From}, wait_apns_connection_up, _) ->
  {keep_state_and_data, {reply, From, ok}};
connected({call, From}, Event, _) when Event =/= gun_pid ->
  {keep_state_and_data, {reply, From, {error, bad_call}}};

connected( cast
         , {push_notification, DeviceId, Notification, Headers}
         , StateData) ->

  #{connection := Connection, queue := Queue, gun_pid := GunConn} = StateData,

  {Conn, Hdrs} = case type(Connection) of 
    token ->
      Connection1 = verify_token(Connection),
      Headers1 = add_authorization_header(Headers, auth_token(Connection1)),
      {Connection1, Headers1};
    _ ->
      {Connection, Headers}
  end,

  HdrsList = get_headers(Hdrs),
  Path = get_device_path(DeviceId),
  _StreamRef = gun:post(GunConn, Path, HdrsList, Notification),
  ApnsId = find_header_val(HdrsList, apns_id),

  Queue1 = lists:sublist([{ApnsId, DeviceId} | Queue], 100),
  StateData1 = StateData#{connection => Conn, queue => Queue1},
  {keep_state, StateData1};

connected( info
         , {gun_response, _, _, fin, Status, Headers}
         , #{ queue := Queue} = StateData) ->
  ?DEBUG("Final packet: ~p~n", [{Status, Headers}]),  
  ApnsId = find_header_val(Headers, apns_id),
  Queue1 = lists:keydelete(ApnsId, 1, Queue),
  StateData1 = StateData#{queue => Queue1},
  {keep_state, StateData1};

connected( info
         , {gun_response, _, StreamRef, nofin, Status, Headers}
         , StateData) ->
  #{connection := Connection, queue := Queue, gun_pid := GunConn} = StateData,
  #{name := Name, timeout := Timeout, feedback := Feedback} = Connection,
  ApnsId = find_header_val(Headers, apns_id),
  Queue1 = lists:keydelete(ApnsId, 1, Queue),
  case gun:await_body(GunConn, StreamRef, Timeout) of
      {ok, Body} ->
          ?DEBUG("Received Data: packet: ~p~n", [{Status, Headers, Body}]),
          case {Status, Feedback, lists:keyfind(ApnsId, 1, Queue)} of
              {_, _ , false} -> ok;
              {410, {M, F}, DeviceId} ->
                  BodyJson = jsx:decode(Body, [return_maps]),
                  #{timestamp := Timestamp} = BodyJson,
                  catch erlang:apply(M, F, [Name, DeviceId, Timestamp]);
              _ ->
                  ok
          end;
      {error, Reason} ->
        ?ERROR_MSG("Error Reading Body ~p~n", [{Status, Headers, Reason}])
  end,
  StateData1 = StateData#{queue => Queue1},
  {keep_state, StateData1};

connected(EventType, EventContent, StateData) ->
  handle_common(EventType, EventContent, ?FUNCTION_NAME, StateData, drop).

-spec down(_, _, _) -> _.
down(internal
    , _
    , #{ gun_pid         := GunPid
       , gun_monitor     := GunMon
       , client          := Client
       , backoff         := Backoff
       , backoff_ceiling := Ceiling
       }) ->
  true = demonitor(GunMon, [flush]),
  gun:close(GunPid),
  Client ! {reconnecting, self()},
  Sleep = backoff(Backoff, Ceiling) * 1000,
  {keep_state_and_data, {state_timeout, Sleep, backoff}};
down(state_timeout, backoff, StateData) ->
  {next_state, open_connection, StateData,
    {next_event, internal, init}};
down(EventType, EventContent, StateData) ->
  handle_common(EventType, EventContent, ?FUNCTION_NAME, StateData, postpone).

-spec handle_common(_, _, _, _, _) -> _.
handle_common({call, From}, gun_pid, _, #{gun_pid := GunPid}, _) ->
  {keep_state_and_data, {reply, From, GunPid}};
handle_common(cast, stop, _, _, _) ->
  {stop, normal};
handle_common( info
             , {'DOWN', GunMon, process, GunPid, Reason}
             , StateName
             , #{gun_pid := GunPid, gun_monitor := GunMon} = StateData
             , _) ->
  {next_state, down, StateData,
    {next_event, internal, {down, StateName, Reason}}};
handle_common( state_timeout
             , EventContent
             , StateName
             , #{gun_pid := GunPid} = StateData
             , _) ->
  gun:close(GunPid),
  {next_state, down, StateData,
    {next_event, internal, {state_timeout, StateName, EventContent}}};
handle_common(_, _, _, _, postpone) ->
  {keep_state_and_data, postpone};
handle_common(_, _, _, _, drop) ->
  keep_state_and_data.

-spec code_change(OldVsn :: term() | {down, term()}
                 , StateName
                 , StateData
                 , Extra :: term()
                 ) -> {ok, StateName, StateData}.
code_change(_OldVsn, StateName, StateData, _Extra) ->
  {ok, StateName, StateData}.

%%%===================================================================
%%% Connection getters/setters Functions
%%%===================================================================

-spec name(connection()) -> name().
name(#{name := ConnectionName}) ->
  ConnectionName.

host(development) ->
  "api.development.push.apple.com";
host(production) ->
  "api.push.apple.com";
host(#{apple_host := Host}) ->
  Host.

-spec port(connection()) -> inet:port_number().
port(#{apple_port := Port}) ->
  Port.

-spec certdata(connection()) -> binary().
certdata(#{certdata := Cert}) ->
  Cert.

-spec certfile(connection()) -> path().
certfile(#{certfile := Certfile}) ->
  Certfile.

-spec keydata(connection()) -> keydata().
keydata(#{keydata := Key}) ->
  Key.

-spec keyfile(connection()) -> path().
keyfile(#{keyfile := Keyfile}) ->
  Keyfile.

-spec type(connection()) -> type().
type(#{type := Type}) ->
  Type.

-spec auth_token(connection()) -> binary().
auth_token(#{jwt_token := Token}) ->
  Token.

-spec proxy(connection()) -> proxy_info() | undefined.
proxy(#{proxy_info := Proxy}) ->
  Proxy;
proxy(_) ->
  undefined.

uuid() ->
  uuid:uuid_to_string(uuid:get_v4(), binary_standard).

transport_opts(Connection) ->
  case type(Connection) of
    certdata ->
      Cert = certdata(Connection),
      Key = keydata(Connection),
      [{cert, Cert}, {key, Key}];
    cert ->
      Certfile = certfile(Connection),
      Keyfile = keyfile(Connection),
      [{certfile, Certfile}, {keyfile, Keyfile}];
    token ->
      CaCertFile = filename:join([code:priv_dir(apns), "GeoTrust_Global_CA.pem"]),
      [{cacertfile, CaCertFile},
       {server_name_indication, disable},
       {crl_check, false},
       {verify, verify_none}]
  end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

-spec get_headers(apns:headers()) -> list().
get_headers(Headers) ->
  List = [ {<<"apns-id">>, apns_id, undefined}
         , {<<"apns-expiration">>, apns_expiration, <<"0">>}
         , {<<"apns-priority">>, apns_priority, <<"5">>}
         , {<<"apns-topic">>, apns_topic, undefined}
         , {<<"apns-collapse_id">>, apns_collapse_id, undefined}
         , {<<"authorization">>, apns_auth_token, undefined}
         ],
  F = fun({ActualHeader, Key, Def}) ->
      case {Key, maps:get(Key, Headers, Def)} of
          {apns_id, undefined} -> [{ActualHeader, uuid()}];
          {_, undefined} -> [];
          {_, Value} -> [{ActualHeader, Value}]
    end
  end,
  lists:flatmap(F, List).

find_header_val(Headers, apns_id) -> find_header_val(Headers, <<"apns-id">>);

find_header_val(Headers, Key) when is_list(Headers) ->
  case lists:keysearch(Key, 1, Headers) of
    {value, {Key, Val}} -> Val;
    _ -> undefined
  end;
find_header_val(Headers, Key) when is_map(Headers) ->
  map:get(Key, Headers, undefined).

-spec get_device_path(apns:device_id()) -> binary().
get_device_path(DeviceId) ->
  <<"/3/device/", DeviceId/binary>>.

-spec add_authorization_header(apns:headers(), apnd:token()) -> apns:headers().
add_authorization_header(Headers, Token) ->
  Headers#{apns_auth_token => <<"bearer ", Token/binary>>}.


-spec backoff(non_neg_integer(), non_neg_integer()) -> non_neg_integer().
backoff(N, Ceiling) ->
  case (math:pow(2, N) - 1) of
    R when R > Ceiling ->
      Ceiling;
    NextN ->
      NString = float_to_list(NextN, [{decimals, 0}]),
      list_to_integer(NString)
  end.
