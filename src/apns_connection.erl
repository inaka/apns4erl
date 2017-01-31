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
-export([ start_link/1
        , default_connection/1
        , name/1
        , host/1
        , port/1
        , certfile/1
        , keyfile/1
        , gun_connection/1
        , close_connection/1
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
             ]).

-type name()         :: atom().
-type host()         :: string() | inet:ip_address().
-type path()         :: string().
-opaque connection() :: #{ name       := name()
                         , apple_host := host()
                         , apple_port := inet:port_number()
                         , certfile   => path()
                         , keyfile    => path()
                         }.

-type state()        :: #{ connection     := connection()
                         , gun_connection := pid()
                         , gun_monitor    := reference()
                         }.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc starts the gen_server
-spec start_link(connection()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(Connection) ->
  Name = name(Connection),
  gen_server:start_link({local, Name}, ?MODULE, Connection, []).

%% @doc Builds a connection() map from the environment variables.
-spec default_connection(name()) -> connection().
default_connection(ConnectionName) ->
  {ok, Host} = application:get_env(apns, apple_host),
  {ok, Port} = application:get_env(apns, apple_port),
  {ok, Certfile} = application:get_env(apns, certfile),
  {ok, Keyfile} = application:get_env(apns, keyfile),

  #{ name       => ConnectionName
   , apple_host => Host
   , apple_port => Port
   , certfile   => Certfile
   , keyfile    => Keyfile
  }.

%% @doc Close the connection with APNs gracefully
-spec close_connection(name()) -> ok.
close_connection(ConnectionName) ->
  gen_server:cast(ConnectionName, stop).

%% @doc Returns the gun's connection PID. This function is only used in tests.
-spec gun_connection(name()) -> pid().
gun_connection(ConnectionName) ->
  gen_server:call(ConnectionName, gun_connection).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init(connection()) -> {ok, State :: state()}.
init(Connection) ->
  {GunMonitor, GunConnectionPid} = open_gun_connection(Connection),
  {ok, #{ connection     => Connection
        , gun_connection => GunConnectionPid
        , gun_monitor    => GunMonitor
        }}.

-spec handle_call( Request :: term()
                 , From    :: {pid()
                 , Tag     :: term()}
                 , State
                 ) -> {reply, ok, State}.
handle_call(gun_connection, _From, #{gun_connection := GunConn} = State) ->
  {reply, GunConn, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

-spec handle_cast(Request :: term(), State) ->
  {noreply, State}.
handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast(_Request, State) ->
  {noreply, State}.

-spec handle_info(Info :: timeout() | term(), State) ->
  {noreply, State}.
handle_info( {'DOWN', GunMonitor, process, GunConnPid, _}
           , #{ gun_connection := GunConnPid
              , gun_monitor    := GunMonitor
              , connection     := Connection
              } = State) ->
  {GunMonitor2, GunConnPid2} = open_gun_connection(Connection),
  {noreply, State#{gun_connection => GunConnPid2, gun_monitor => GunMonitor2}};
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

%%%===================================================================
%%% Internal Functions
%%%===================================================================

-spec open_gun_connection(connection()) -> { GunMonitor       :: reference()
                                           , GunConnectionPid :: pid()
                                           }.
open_gun_connection(Connection) ->
  Certfile = certfile(Connection),
  Keyfile = keyfile(Connection),
  Host = host(Connection),
  Port = port(Connection),
  TransportOpts = [{certfile, Certfile}, {keyfile, Keyfile}],
  {ok, GunConnectionPid} = gun:open( Host
                                   , Port
                                   , #{ protocols => [http2]
                                      , transport_opts => TransportOpts
                                      }),
  GunMonitor = monitor(process, GunConnectionPid),
  {GunMonitor, GunConnectionPid}.
