-module(connection_SUITE).
-author("Felipe Ripoll <ferigis@inakanetworks.com>").

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        ]).

-export([ default_connection/1
        , connect/1
        , connect_timeout/1
        , gun_connection_lost/1
        , gun_connection_lost_timeout/1
        , push_notification/1
        , push_notification_token/1
        , push_notification_timeout/1
        , default_headers/1
        , test_coverage/1
        ]).

-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() -> [atom()].
all() ->  [ default_connection
          , connect
          , connect_timeout
          , gun_connection_lost
          , gun_connection_lost_timeout
          , push_notification
          , push_notification_token
          , push_notification_timeout
          , default_headers
          , test_coverage
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

  % cert type connection
  DefaultConnection = apns_connection:default_connection(cert, ConnectionName),
  ConnectionName = apns_connection:name(DefaultConnection),
  Host = apns_connection:host(DefaultConnection),
  Port = apns_connection:port(DefaultConnection),
  Certfile = apns_connection:certfile(DefaultConnection),
  Keyfile = apns_connection:keyfile(DefaultConnection),
  cert = apns_connection:type(DefaultConnection),

  % token type connection
  DefaultConnection2 =
    apns_connection:default_connection(token, ConnectionName),
  ConnectionName = apns_connection:name(DefaultConnection2),
  Host = apns_connection:host(DefaultConnection2),
  Port = apns_connection:port(DefaultConnection2),
  token = apns_connection:type(DefaultConnection2),
  ok.

-spec connect(config()) -> ok.
connect(_Config) ->
  ok = mock_gun_open(),
  ok = mock_gun_await_up({ok, http2}),
  ConnectionName = my_connection,
  {ok, ServerPid}  = apns:connect(cert, ConnectionName),
  true = is_process_alive(ServerPid),
  ServerPid = whereis(ConnectionName),
  ok = close_connection(ConnectionName),
  [_] = meck:unload(),
  ok.

-spec connect_timeout(config()) -> ok.
connect_timeout(_Config) ->
  ok = mock_gun_open(),
  ok = mock_gun_await_up({error, timeout}),
  ConnectionName = my_connection,
  {error, timeout}  = apns:connect(token, ConnectionName),
  ok = close_connection(ConnectionName),
  [_] = meck:unload(),
  ok.

-spec gun_connection_lost(config()) -> ok.
gun_connection_lost(_Config) ->
  ok = mock_gun_open(),
  ok = mock_gun_await_up({ok, http2}),
  ConnectionName = my_connection2,
  {ok, ServerPid}  = apns:connect(cert, ConnectionName),
  GunPid = apns_connection:gun_connection(ConnectionName),
  true = is_process_alive(GunPid),
  GunPid ! {crash, ServerPid},
  ktn_task:wait_for(fun() -> is_process_alive(GunPid) end, false),
  ktn_task:wait_for(fun() ->
      apns_connection:gun_connection(ConnectionName) == GunPid
    end, false),
  GunPid2 = apns_connection:gun_connection(ConnectionName),
  true = is_process_alive(GunPid2),
  true = (GunPid =/= GunPid2),
  ok = close_connection(ConnectionName),
  ktn_task:wait_for(fun() -> is_process_alive(GunPid2) end, false),
  [_] = meck:unload(),
  ok.

-spec gun_connection_lost_timeout(config()) -> ok.
gun_connection_lost_timeout(_Config) ->
  ok = mock_gun_open(),
  ok = mock_gun_await_up({ok, http2}),
  ConnectionName = my_connection,
  {ok, _ServerPid}  = apns:connect(cert, ConnectionName),
  GunPid = apns_connection:gun_connection(ConnectionName),

  ok = mock_gun_await_up({error, timeout}),
  ok = meck:expect(gun, close, fun(_) ->
    ok
  end),
  ConnectionName ! reconnect,

  ktn_task:wait_for(fun() ->
      apns_connection:gun_connection(ConnectionName) == GunPid
    end, false),

  GunPid2 = apns_connection:gun_connection(ConnectionName),

  ktn_task:wait_for(fun() ->
      apns_connection:gun_connection(ConnectionName) == GunPid2
    end, false),

  ok = close_connection(ConnectionName),
  [_] = meck:unload(),
  ok.

-spec push_notification(config()) -> ok.
push_notification(_Config) ->
  ok = mock_gun_open(),
  ok = mock_gun_await_up({ok, http2}),
  ConnectionName = my_connection,
  {ok, _ApnsPid} = apns:connect(cert, ConnectionName),
  Headers = #{ apns_id          => <<"apnsid">>
             , apns_expiration  => <<"0">>
             , apns_priority    => <<"10">>
             , apns_topic       => <<"net.inaka.myapp">>
             },
  Notification = #{<<"aps">> => #{<<"alert">> => <<"you have a message">>}},
  DeviceId = <<"device_id">>,
  ok = mock_gun_post(),
  ResponseCode = 200,
  ResponseHeaders = [{<<"apns-id">>, <<"apnsid">>}],
  ok = mock_gun_await({response, fin, ResponseCode, ResponseHeaders}),
  {ResponseCode, ResponseHeaders, no_body} =
    apns:push_notification(ConnectionName, DeviceId, Notification, Headers),

  %% Now mock an error from APNs
  [_] = meck:unload(),
  ok = mock_gun_post(),
  ErrorCode = 400,
  ErrorHeaders = [{<<"apns-id">>, <<"apnsid2">>}],
  ErrorBody = <<"{\"reason\":\"BadTopic\"}">>,
  ok = mock_gun_await({response, nofin, ErrorCode, ErrorHeaders}),
  ok = mock_gun_await_body(ErrorBody),

  {ErrorCode, ErrorHeaders, _DecodedErrorBody} =
    apns:push_notification(ConnectionName, DeviceId, Notification),
  ok = close_connection(ConnectionName),
  [_] = meck:unload(),
  ok.

-spec push_notification_token(config()) -> ok.
push_notification_token(_Config) ->
  ok = mock_gun_open(),
  ok = mock_gun_await_up({ok, http2}),
  ConnectionName = my_token_connection,
  {ok, _ApnsPid} = apns:connect(token, ConnectionName),
  Headers = #{ apns_id          => <<"apnsid2">>
             , apns_expiration  => <<"0">>
             , apns_priority    => <<"10">>
             , apns_topic       => <<"net.inaka.myapp">>
             },
  Notification = #{<<"aps">> => #{<<"alert">> => <<"more messages">>}},
  DeviceId = <<"device_id2">>,
  Token = apns:generate_token(<<"TeamId">>, <<"KeyId">>),
  ok = mock_gun_post(),
  ResponseCode = 200,
  ResponseHeaders = [{<<"apns-id">>, <<"apnsid2">>}],
  ok = mock_gun_await({response, fin, ResponseCode, ResponseHeaders}),
  {ResponseCode, ResponseHeaders, no_body} =
    apns:push_notification_token( ConnectionName
                                , Token
                                , DeviceId
                                , Notification
                                , Headers
                                ),
  {ResponseCode, ResponseHeaders, no_body} =
    apns:push_notification_token( ConnectionName
                                , Token
                                , DeviceId
                                , Notification
                                ),
  ok = close_connection(ConnectionName),
  [_] = meck:unload(),
  ok.

-spec push_notification_timeout(config()) -> ok.
push_notification_timeout(_Config) ->
  %% Change the timeout variable
  {ok, OriginalTimeout} = application:get_env(apns, timeout),
  ok = application:set_env(apns, timeout, 0),

  ok = mock_gun_open(),
  ok = mock_gun_await_up({ok, http2}),
  ok = mock_gun_post(),
  ConnectionName = my_connection,
  {ok, _ApnsPid} = apns:connect(cert, ConnectionName),
  Notification = #{<<"aps">> => #{<<"alert">> => <<"another message">>}},
  DeviceId = <<"device_id">>,
  timeout = apns:push_notification(ConnectionName, DeviceId, Notification),
  ok = close_connection(ConnectionName),
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

-spec test_coverage(config()) -> ok.
test_coverage(_Config) ->
  ok = mock_gun_open(),
  ok = mock_gun_await_up({ok, http2}),
  ConnectionName = my_connection,
  {ok, _ServerPid}  = apns:connect(cert, ConnectionName),

  ok = gen_server:call(ConnectionName, hello_call),
  ok = gen_server:cast(ConnectionName, hello_cast),
  ConnectionName ! hello_info,
  {ok, #{}} = apns_connection:code_change(old_version, #{}, []),

  10 = apns_connection:backoff(17, 10),

  ok = close_connection(ConnectionName),
  [_] = meck:unload(),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec test_function() -> ok.
test_function() ->
  receive
    normal           -> ok;
    {crash, Pid}     -> Pid ! {gun_down, self(), http2, closed, [], []};
    _                -> test_function()
  end.

-spec mock_gun_open() -> ok.
mock_gun_open() ->
  meck:expect(gun, open, fun(_, _, _) ->
    % Return a Pid but nothing special with it
    {ok, spawn(fun test_function/0)}
  end).


-spec mock_gun_post() -> ok.
mock_gun_post() ->
  meck:expect(gun, post, fun(_, _, _, _) ->
    make_ref()
  end).

-spec mock_gun_await(term()) -> ok.
mock_gun_await(Result) ->
  meck:expect(gun, await, fun(_, _, _) ->
    Result
  end).

-spec mock_gun_await_body(term()) -> ok.
mock_gun_await_body(Body) ->
  meck:expect(gun, await_body, fun(_, _, _) ->
    {ok, Body}
  end).

-spec mock_gun_await_up(term()) -> ok.
mock_gun_await_up(Result) ->
  meck:expect(gun, await_up, fun(_, _) ->
    Result
  end).

close_connection(ConnectionName) ->
  ok = apns:close_connection(ConnectionName),
  ktn_task:wait_for(fun() ->
      is_process_alive(whereis(ConnectionName))
    end, false),
  ok.
