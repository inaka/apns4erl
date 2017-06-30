-module(connection_SUITE).
-author("Felipe Ripoll <ferigis@inakanetworks.com>").

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        ]).

-export([ default_connection/1
        , certdata_keydata_connection/1
        , connect/1
        , connect_without_name/1
        , http2_connection_lost/1
        , push_notification/1
        , push_notification_token/1
        , push_notification_timeout/1
        , restrict_calls_to_owner/1
        , default_headers/1
        , test_coverage/1
        ]).

-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() -> [atom()].
all() ->  [ default_connection
          , certdata_keydata_connection
          , connect
          , connect_without_name
          , http2_connection_lost
          , push_notification
          , push_notification_token
          , push_notification_timeout
          , restrict_calls_to_owner
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

-spec certdata_keydata_connection(config()) -> ok.
certdata_keydata_connection(_Config) ->
  ConnectionName = my_connection2,
  {ok, Host} = application:get_env(apns, apple_host),
  {ok, Port} = application:get_env(apns, apple_port),
  {ok, Certdata} = application:get_env(apns, certdata),
  {ok, Keydata} = application:get_env(apns, keydata),

  % certdata type connection
  DefaultConnection = apns_connection:default_connection(certdata, ConnectionName),
  ConnectionName = apns_connection:name(DefaultConnection),
  Host = apns_connection:host(DefaultConnection),
  Port = apns_connection:port(DefaultConnection),
  Certdata = apns_connection:certdata(DefaultConnection),
  Keydata = apns_connection:keydata(DefaultConnection),
  certdata = apns_connection:type(DefaultConnection),
  ok.

-spec connect(config()) -> ok.
connect(_Config) ->
  ok = mock_open_http2_connection(),
  ConnectionName = my_connection,
  {ok, ServerPid}  = apns:connect(cert, ConnectionName),
  true = is_process_alive(ServerPid),
  ServerPid = whereis(ConnectionName),
  ok = close_connection(ConnectionName),
  [_] = meck:unload(),
  ok.

-spec connect_without_name(config()) -> ok.
connect_without_name(_Config) ->
  ok = mock_open_http2_connection(),
  ConnectionName = undefined,
  {ok, ServerPid}  = apns:connect(cert, ConnectionName),
  true = is_process_alive(ServerPid),
  ok = close_connection(ServerPid),
  [_] = meck:unload(),
  ok.

-spec http2_connection_lost(config()) -> ok.
http2_connection_lost(_Config) ->
  ok = mock_open_http2_connection(),
  ConnectionName = my_connection3,
  {ok, ServerPid}  = apns:connect(cert, ConnectionName),

  HTTP2Conn = apns_connection:http2_connection(ConnectionName),
  HTTP2Conn = apns_connection:http2_connection(ServerPid),

  true = is_process_alive(HTTP2Conn),
  HTTP2Conn ! {crash, ServerPid},
  ktn_task:wait_for(fun() -> is_process_alive(HTTP2Conn) end, false),
  ktn_task:wait_for(fun() ->
      apns_connection:http2_connection(ConnectionName) == HTTP2Conn
    end, false),
  HTTP2Conn2 = apns_connection:http2_connection(ConnectionName),
  true = is_process_alive(HTTP2Conn2),
  true = (HTTP2Conn =/= HTTP2Conn2),

  % Repeat with ceiling 0, for testing coverage
  ok = application:set_env(apns, backoff_ceiling, 0),

  ConnectionName2 = my_connection4,
  {ok, ServerPid2}  = apns:connect(cert, ConnectionName2),
  HTTP2Conn3 = apns_connection:http2_connection(ConnectionName2),
  true = is_process_alive(HTTP2Conn3),

  HTTP2Conn3 ! {crash, ServerPid2},
  ktn_task:wait_for(fun() -> is_process_alive(HTTP2Conn3) end, false),
  ktn_task:wait_for(fun() ->
      apns_connection:http2_connection(ConnectionName2) == HTTP2Conn3
    end, false),
  HTTP2Conn4 = apns_connection:http2_connection(ConnectionName2),
  true = is_process_alive(HTTP2Conn4),
  true = (HTTP2Conn3 =/= HTTP2Conn4),

  ok = application:unset_env(apns, backoff_ceiling),
  ok = close_connection(ConnectionName),
  ktn_task:wait_for(fun() -> is_process_alive(HTTP2Conn2) end, false),
  [_] = meck:unload(),
  ok.

-spec push_notification(config()) -> ok.
push_notification(_Config) ->
  ok = mock_open_http2_connection(),
  ConnectionName = my_connection,
  {ok, ServerPid} = apns:connect(cert, ConnectionName),
  Headers = #{ apns_id          => <<"apnsid">>
             , apns_expiration  => <<"0">>
             , apns_priority    => <<"10">>
             , apns_topic       => <<"net.inaka.myapp">>
             },
  Notification = #{<<"aps">> => #{<<"alert">> => <<"you have a message">>}},
  DeviceId = <<"device_id">>,
  ok = mock_http2_post(),
  ResponseCode = 200,
  ResponseHeaders = [{<<"apns-id">>, <<"apnsid">>}],
  ok = mock_http2_get_response(ResponseCode, ResponseHeaders, []),

  {ResponseCode, ResponseHeaders, no_body} =
    apns:push_notification(ConnectionName, DeviceId, Notification, Headers),

  %% Now mock an error from APNs
  ErrorCode = 400,
  ErrorHeaders = [{<<"apns-id">>, <<"apnsid2">>}],
  ErrorBody = [<<"{\"reason\":\"BadDeviceToken\"}">>],

  ok = mock_http2_get_response(ErrorCode, ErrorHeaders, ErrorBody),

  {ErrorCode, ErrorHeaders, ErrorBodyDecoded} =
    apns:push_notification(ConnectionName, DeviceId, Notification),
  {ErrorCode, ErrorHeaders, ErrorBodyDecoded} =
    apns:push_notification(ServerPid, DeviceId, Notification),

  ok = close_connection(ConnectionName),
  [_] = meck:unload(),
  ok.

-spec push_notification_token(config()) -> ok.
push_notification_token(_Config) ->
  ok = mock_open_http2_connection(),
  ConnectionName = my_token_connection,
  {ok, ServerPid} = apns:connect(token, ConnectionName),
  Headers = #{ apns_id          => <<"apnsid2">>
             , apns_expiration  => <<"0">>
             , apns_priority    => <<"10">>
             , apns_topic       => <<"net.inaka.myapp">>
             },
  Notification = #{<<"aps">> => #{<<"alert">> => <<"more messages">>}},
  DeviceId = <<"device_id2">>,
  ok = mock_http2_post(),
  ok = maybe_mock_apns_os(),
  Token = apns:generate_token(<<"TeamId">>, <<"KeyId">>),

  ResponseCode = 200,
  ResponseHeaders = [{<<"apns-id">>, <<"apnsid2">>}],
  ok = mock_http2_get_response(ResponseCode, ResponseHeaders, []),
  {ResponseCode, ResponseHeaders, no_body} =
    apns:push_notification_token( ConnectionName
                                , Token
                                , DeviceId
                                , Notification
                                , Headers
                                ),
  {ResponseCode, ResponseHeaders, _Body} =
    apns:push_notification_token( ServerPid
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

  ok = close_connection(ServerPid),
  _ = meck:unload(),
  ok.

-spec restrict_calls_to_owner(config()) -> ok.
restrict_calls_to_owner(_Config) ->
  ok = mock_open_http2_connection(),
  ok = mock_http2_post(),
  Self = self(),

  SpawnedPid = spawn(fun() ->
    {ok, Pid} = apns:connect(cert, undefined),
    Self ! {self(), Pid}
  end),

  ConnectionPid = receive
    {SpawnedPid, ServerPid} -> ServerPid
  end,

  {error, not_connection_owner} = apns:push_notification(ConnectionPid, <<"device_id">>, #{}, #{}),
  {error, not_connection_owner} =
    apns:push_notification_token(ConnectionPid, <<"token">>, <<"device_id">>, #{}, #{}),

  ok = close_connection(ConnectionPid),
  [_] = meck:unload(),
  ok.

-spec push_notification_timeout(config()) -> ok.
push_notification_timeout(_Config) ->
  %% Change the timeout variable
  {ok, OriginalTimeout} = application:get_env(apns, timeout),
  ok = application:set_env(apns, timeout, 0),

  ok = mock_open_http2_connection(),
  ok = mock_http2_post(),
  ok = mock_http2_get_response(200, [{<<"apns-id">>, <<"apnsid">>}], []),
  ConnectionName = my_connection,
  {ok, _ApnsPid} = apns:connect(cert, ConnectionName),
  Notification = #{<<"aps">> => #{<<"alert">> => <<"another message">>}},
  DeviceId = <<"device_id">>,
  {timeout, _} = apns:push_notification(ConnectionName, DeviceId, Notification),
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
  ok = mock_open_http2_connection(),
  ConnectionName = my_connection,
  {ok, _ServerPid}  = apns:connect(cert, ConnectionName),

  ok = gen_server:call(ConnectionName, hello_call),
  ok = gen_server:cast(ConnectionName, hello_cast),
  ConnectionName ! hello_info,
  {ok, #{}} = apns_connection:code_change(old_version, #{}, []),

  ok = close_connection(ConnectionName),
  [_] = meck:unload(),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec test_function() -> ok.
test_function() ->
  receive
    normal       -> ok;
    {crash, Pid} -> Pid ! {'EXIT', self(), no_reason};
    _            -> test_function()
  end.

-spec mock_open_http2_connection() -> ok.
mock_open_http2_connection() ->
  meck:expect(h2_client, start_link, fun(https, _, _) ->
    % Return a Pid but nothing special with it
    {ok, spawn(fun test_function/0)}
  end).

-spec mock_http2_post() -> ok.
mock_http2_post() ->
  meck:expect(h2_client, send_request, fun(_, _, _) ->
    self() ! {'END_STREAM', 1},
    {ok, 1}
  end).

-spec mock_http2_get_response(integer(), list(), list()) -> ok.
mock_http2_get_response(ResponseCode, ResponseHeaders, Body) ->
  meck:expect(h2_client, get_response, fun(_, _) ->
    {ok, {[{<<":status">>, integer_to_binary(ResponseCode)} | ResponseHeaders], Body}}
  end).

-spec maybe_mock_apns_os() -> ok.
maybe_mock_apns_os() ->
  %% @TODO: Add logic to validate if the user wants to avoid to mock this call,
  %% and make the real call with real files instead.
  meck:expect(apns_os, cmd, fun(_) ->
    {0, "12345678"}
  end).

close_connection(ConnectionId) when is_atom(ConnectionId) ->
  ConnectionPid = whereis(ConnectionId),
  close_connection(ConnectionPid);
close_connection(ConnectionId) when is_pid(ConnectionId) ->
  ok = apns:close_connection(ConnectionId),
  ktn_task:wait_for(fun() ->
      is_process_alive(ConnectionId)
    end, false),
  ok.
