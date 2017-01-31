-module(connection_SUITE).
-author("Felipe Ripoll <ferigis@inakanetworks.com>").

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        ]).

-export([ default_connection/1
        , connect/1
        , gun_connection_crashes/1
        , push_notification/1
        , push_notification_timeout/1
        , default_headers/1
        ]).

-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() -> [atom()].
all() ->  [ default_connection
          , connect
          , gun_connection_crashes
          , push_notification
          , push_notification_timeout
          , default_headers
          ].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  ok = apns:start(),
  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  ok = apns:stop(),
  Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test Cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec default_connection(config()) -> ok.
default_connection(_Config) ->
  ConnectionName = my_connection,
  {ok, Host} = application:get_env(apns, apple_host),
  {ok, Port} = application:get_env(apns, apple_port),
  {ok, Certfile} = application:get_env(apns, certfile),
  {ok, Keyfile} = application:get_env(apns, keyfile),
  DefaultConnection = apns_connection:default_connection(ConnectionName),
  ConnectionName = apns_connection:name(DefaultConnection),
  Host = apns_connection:host(DefaultConnection),
  Port = apns_connection:port(DefaultConnection),
  Certfile = apns_connection:certfile(DefaultConnection),
  Keyfile = apns_connection:keyfile(DefaultConnection),
  ok.

-spec connect(config()) -> ok.
connect(_Config) ->
  ConnectionName = my_connection,
  {ok, ServerPid}  = apns:connect(ConnectionName),
  true = is_process_alive(ServerPid),
  ServerPid = whereis(ConnectionName),
  ok = apns:close_connection(ConnectionName),
  ktn_task:wait_for(fun() -> whereis(ConnectionName) end, undefined),
  ok.

-spec gun_connection_crashes(config()) -> ok.
gun_connection_crashes(_Config) ->
  ok = meck:expect(gun, open, fun(_, _, _) ->
      {ok, spawn(fun crash/0)}
    end),
  ConnectionName = my_connection2,
  {ok, _ServerPid}  = apns:connect(ConnectionName),
  GunPid = apns_connection:gun_connection(ConnectionName),
  true = is_process_alive(GunPid),
  GunPid ! crash,
  ktn_task:wait_for(fun() -> is_process_alive(GunPid) end, false),
  GunPid2 = apns_connection:gun_connection(ConnectionName),
  true = is_process_alive(GunPid2),
  true = (GunPid =/= GunPid2),
  ok = apns:close_connection(ConnectionName),
  ktn_task:wait_for(fun() -> is_process_alive(GunPid2) end, false),
  [_] = meck:unload(),
  ok.

-spec push_notification(config()) -> ok.
push_notification(_Config) ->
  ok = meck:expect(gun, post, fun(GunConn, _, _, _) ->
      Ref = make_ref(),
      {ok, _} =
        timer:send_after(1000, {gun_response, GunConn, Ref, fin, 200, []}),
      Ref
    end),
  ConnectionName = my_connection,
  {ok, _ApnsPid} = apns:connect(ConnectionName),
  Headers = #{ apns_id          => <<"apnsid">>
             , apns_expiration  => <<"0">>
             , apns_priority    => <<"10">>
             , apns_topic       => <<"net.inaka.myapp">>
             },
  Notification = #{<<"aps">> => #{<<"alert">> => <<"yo have a message">>}},
  DeviceId = "device_id",
  {200, []} =
    apns:push_notification(ConnectionName, DeviceId, Notification, Headers),
  [_] = meck:unload(),

  %% Now mock an error from APNs
  ok = meck:expect(gun, post, fun(GunConn, _, _, _) ->
      Ref = make_ref(),
      {ok, _} = timer:send_after(1000, { gun_response
                                       , GunConn
                                       , Ref
                                       , nofin
                                       , 400
                                       , [{<<"apns-id">>, <<"apnsid2">>}]
                                       }),
      {ok, _} = timer:send_after(1500, { gun_data
                                       , GunConn
                                       , Ref
                                       , fin
                                       , [<<"{\"reason\":\"BadTopic\"}">>]
                                       }),
      Ref
    end),

  {400, _} =
    apns:push_notification(ConnectionName, DeviceId, Notification, Headers),
  ok = apns:close_connection(ConnectionName),
  [_] = meck:unload(),
  ok.

-spec push_notification_timeout(config()) -> ok.
push_notification_timeout(_Config) ->
  %% Change the timeout variable
  {ok, OriginalTimeout} = application:get_env(apns, timeout),
  ok = application:set_env(apns, timeout, 500),

  ok = meck:expect(gun, post, fun(_, _, _, _) ->
      make_ref()
    end),
  ConnectionName = my_connection,
  {ok, _ApnsPid} = apns:connect(ConnectionName),
  Notification = #{<<"aps">> => #{<<"alert">> => <<"another message">>}},
  DeviceId = "device_id",
  timeout = apns:push_notification(ConnectionName, DeviceId, Notification),
  [_] = meck:unload(),

  % code coverage
  ok = meck:expect(gun, post, fun(GunConn, _, _, _) ->
      Ref = make_ref(),
      {ok, _} = timer:send_after(1, { gun_response
                                    , GunConn
                                    , Ref
                                    , nofin
                                    , 400
                                    , [{<<"apns-id">>, <<"apnsid">>}]
                                    }),
      % don't send the second message in order to throw the timeout
      Ref
    end),
  timeout = apns:push_notification(ConnectionName, DeviceId, Notification),
  [_] = meck:unload(),

  % turn back the original timeout
  ok = application:set_env(apns, timeout, OriginalTimeout),
  ok.

-spec default_headers(config()) -> ok.
default_headers(_Config) ->
  % Replace the environment header variables
  {ok, OriginalApnsId} = application:get_env(apns, apns_id),
  {ok, OriginalApnsExp} = application:get_env(apns, apns_expiration),
  {ok, OriginalApnsPriority} = application:get_env(apns, apns_priority),
  {ok, OriginalApnsTopic} = application:get_env(apns, apns_topic),
  {ok, OriginalApnsCollapseId} = application:get_env(apns, apns_collapse_id),

  ApnsId = "this is the ID",
  ApnsExp = 10,
  ApnsPriority = undefined,
  ApnsTopic = <<"com.example.mycoolapp">>,
  ApnsCollapseId = undefined,

  ok = application:set_env(apns, apns_id, ApnsId),
  ok = application:set_env(apns, apns_expiration, ApnsExp),
  ok = application:set_env(apns, apns_priority, ApnsPriority),
  ok = application:set_env(apns, apns_topic, ApnsTopic),
  ok = application:set_env(apns, apns_collapse_id, ApnsCollapseId),

  Expected = #{ apns_id => list_to_binary(ApnsId)
              , apns_expiration => list_to_binary(integer_to_list(ApnsExp))
              , apns_topic => ApnsTopic
              },

  Expected = apns:default_headers(),

  % turn back the original values
  ok = application:set_env(apns, apns_id, OriginalApnsId),
  ok = application:set_env(apns, apns_expiration, OriginalApnsExp),
  ok = application:set_env(apns, apns_priority, OriginalApnsPriority),
  ok = application:set_env(apns, apns_topic, OriginalApnsTopic),
  ok = application:set_env(apns, apns_collapse_id, OriginalApnsCollapseId),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec crash() -> ok.
crash() ->
  receive
    crash -> exit(crashed);
    _     -> ok
  end.
