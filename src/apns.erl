%%-------------------------------------------------------------------
%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%% @copyright (C) 2010 Fernando Benavides <fernando.benavides@inakanetworks.com>
%% @doc Apple Push Notification Server for Erlang
%% @end
%%-------------------------------------------------------------------
-module(apns).
-vsn('0.1').

%% @headerfile "apns.hrl"
-include("apns.hrl").
%% @headerfile "localized.hrl"
-include("localized.hrl").

-export([start/0, stop/0]).
-export([connect/0, connect/1, connect/2, disconnect/1]).
-export([send_badge/3, send_message/2, send_message/3, send_message/4, send_message/5, send_message/6]).

%% @type conn_id() = atom() | pid(). Connection Identifier.
-type conn_id() :: atom() | pid().
-export_type([conn_id/0]).

%% @type alert() = string() | {LocKey::string(), LocArgs::[string()]} | {LocKey::string(), LocArgs::[string()], ActionLocKey::string()}.
%%        Possibly localized alert.
-type alert() :: string() | #loc_alert{}.
-export_type([alert/0]).

%% @doc Starts the application
%% @spec start() -> ok | {error, {already_started, apns}}
-spec start() -> ok | {error, {already_started, apns}}.
start() ->
  _ = application:start(public_key),
  _ = application:start(ssl),
  application:start(apns).

%% @doc Stops the application
%% @spec stop() -> ok
-spec stop() -> ok.
stop() ->
  application:stop(apns).

%% @doc Opens an unnamed connection using the default parameters
%% @spec connect() -> {ok, pid()} | {error, Reason::term()}
-spec connect() -> {ok, pid()} | {error, Reason::term()}.
connect() ->
  connect(default_connection()).

%% @doc Opens an unnamed connection using the given certificate file
%%      or using the given #apns_connection{} parameters
%%      or the name and default configuration if a name is given
%% @spec connect(atom() | string() | #apns_connection{}) -> {ok, pid()} | {error, {already_started, pid()}} | {error, Reason::term()}
-spec connect(atom() | string() | #apns_connection{}) -> {ok, pid()} | {error, {already_started, pid()}} | {error, Reason::term()}.
connect(Name) when is_atom(Name) ->
  connect(Name, #apns_connection{});
connect(Connection) when is_record(Connection, apns_connection) ->
  apns_sup:start_connection(Connection);
connect(CertFile) ->
  connect((default_connection())#apns_connection{cert_file = CertFile}).

%% @doc Opens an connection named after the atom()
%%      using the given certificate file
%%      or using the given #apns_connection{} parameters
%% @spec connect(atom(), string() | #apns_connection{}) -> {ok, pid()} | {error, {already_started, pid()}} | {error, Reason::term()}
-spec connect(atom(), string() | #apns_connection{}) -> {ok, pid()} | {error, {already_started, pid()}} | {error, Reason::term()}.
connect(Name, Connection) when is_record(Connection, apns_connection) ->
  apns_sup:start_connection(Name, Connection);
connect(Name, CertFile) ->
  connect(Name, (default_connection())#apns_connection{cert_file = CertFile}).

%% @doc Closes an open connection
%% @spec disconnect(conn_id()) -> ok
-spec disconnect(conn_id()) -> ok.
disconnect(ConnId) ->
  apns_connection:stop(ConnId).

%% @doc Sends a message to Apple
%% @spec send_message(conn_id(), #apns_msg{}) -> ok
-spec send_message(conn_id(), #apns_msg{}) -> ok.
send_message(ConnId, Msg) ->
  apns_connection:send_message(ConnId, Msg).

%% @doc Sends a message to Apple with just a badge
%% @spec send_badge(conn_id(), Token::string(), Badge::integer()) -> ok
-spec send_badge(conn_id(), string(), integer()) -> ok.
send_badge(ConnId, DeviceToken, Badge) -> 
  send_message(ConnId, #apns_msg{device_token = DeviceToken,
                                 badge = Badge}).

%% @doc Sends a message to Apple with just an alert
%% @spec send_message(conn_id(), Token::string(), Alert::alert()) -> ok
-spec send_message(conn_id(), string(), alert()) -> ok.
send_message(ConnId, DeviceToken, Alert) -> 
  send_message(ConnId, #apns_msg{device_token = DeviceToken,
                                 alert = Alert}).

%% @doc Sends a message to Apple with an alert and a badge
%% @spec send_message(conn_id(), Token::string(), Alert::alert(), Badge::integer()) -> ok
-spec send_message(conn_id(), Token::string(), Alert::alert(), Badge::integer()) -> ok.
send_message(ConnId, DeviceToken, Alert, Badge) -> 
  send_message(ConnId, #apns_msg{device_token = DeviceToken,
                                 badge = Badge,
                                 alert = Alert}).

%% @doc Sends a full message to Apple
%% @spec send_message(conn_id(), Token::string(), Alert::alert(), Badge::integer(), Sound::string()) -> ok
-spec send_message(conn_id(), Token::string(), Alert::alert(), Badge::integer(), Sound::string()) -> ok.
send_message(ConnId, DeviceToken, Alert, Badge, Sound) -> 
  send_message(ConnId, #apns_msg{alert = Alert,
                                 badge = Badge,
                                 sound = Sound,
                                 device_token = DeviceToken}).

%% @doc Sends a full message to Apple with extra arguments
%% @spec send_message(conn_id(), Token::string(), Alert::alert(), Badge::integer(), Sound::string(), ExtraArgs::[apns_mochijson2:json_property()]) -> ok
-spec send_message(conn_id(), Token::string(), Alert::alert(), Badge::integer(), Sound::string(), ExtraArgs::[apns_mochijson2:json_property()]) -> ok.
send_message(ConnId, DeviceToken, Alert, Badge, Sound, ExtraArgs) -> 
  send_message(ConnId, #apns_msg{alert = Alert,
                                 badge = Badge,
                                 sound = Sound,
                                 extra = ExtraArgs,
                                 device_token = DeviceToken}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_env(K, Def) ->
  case application:get_env(apns, K) of
    {ok, V} -> V;
    _ -> Def
  end.

default_connection() ->
  DefaultConn = #apns_connection{},
  DefaultConn#apns_connection{apple_host  = get_env(apple_host, DefaultConn#apns_connection.apple_host),
                              apple_port  = get_env(apple_port, DefaultConn#apns_connection.apple_port),
                              cert_file   = get_env(cert_file,  DefaultConn#apns_connection.cert_file),
                              ssl_seed    = get_env(ssl_seed,   DefaultConn#apns_connection.ssl_seed),
                              timeout     = get_env(timeout,    DefaultConn#apns_connection.timeout)}.
