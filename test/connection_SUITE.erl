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
        , gun_connection_lost/1
        , gun_connection_lost_timeout/1
        , gun_connection_killed/1
        , push_notification/1
        , push_notification_token/1
        , push_notification_json_body/1
        , generate_token_jwt_claims/1
        , push_notification_from_any_process/1
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
          , certdata_keydata_connection
          , connect
          , connect_without_name
          , gun_connection_lost
          , gun_connection_lost_timeout
          , gun_connection_killed
          , push_notification
          , push_notification_token
          , push_notification_json_body
          , generate_token_jwt_claims
          , push_notification_from_any_process
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
  ConnectionName = ?FUNCTION_NAME,
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
  ConnectionName = ?FUNCTION_NAME,
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
  ok = mock_gun_open(),
  ConnectionName = ?FUNCTION_NAME,
  {ok, ServerPid}  = apns:connect(cert, ConnectionName),
  true = is_process_alive(ServerPid),
  ServerPid = whereis(ConnectionName),
  ok = close_connection(ConnectionName),
  [_] = meck:unload(),
  ok.

-spec connect_without_name(config()) -> ok.
connect_without_name(_Config) ->
  ok = mock_gun_open(),
  ConnectionName = undefined,
  {ok, ServerPid}  = apns:connect(cert, ConnectionName),
  true = is_process_alive(ServerPid),
  ok = close_connection(ServerPid),
  [_] = meck:unload(),
  ok.

-spec gun_connection_lost(config()) -> ok.
gun_connection_lost(_Config) ->
  ok = mock_gun_open(),
  ConnectionName = ?FUNCTION_NAME,
  {ok, ServerPid}  = apns:connect(cert, ConnectionName),
  GunPid = apns_connection:gun_pid(ConnectionName),
  true = is_process_alive(GunPid),
  GunPid ! {crash, ServerPid},
  wait_for(fun() -> is_process_alive(GunPid) end, false),
  wait_for(fun() ->
      apns_connection:gun_pid(ConnectionName) == GunPid
    end, false),
  GunPid2 = apns_connection:gun_pid(ConnectionName),
  true = is_process_alive(GunPid2),
  true = (GunPid =/= GunPid2),
  ok = close_connection(ConnectionName),
  [_] = meck:unload(),
  ok.

-spec gun_connection_lost_timeout(config()) -> ok.
gun_connection_lost_timeout(_Config) ->
  ok = mock_gun_open(),
  ConnectionName = ?FUNCTION_NAME,
  % when backoff is greater than ceiling
  ok = application:set_env(apns, backoff_ceiling, 2),
  {ok, ServerPid}  = apns:connect(cert, ConnectionName),
  GunPid = apns_connection:gun_pid(ConnectionName),
  ok = meck:expect(gun, close, fun(_) ->
    ok
  end),

  GunPid ! {crash, ServerPid},
  wait_for(fun() ->
      apns_connection:gun_pid(ConnectionName) == GunPid
    end, false),

  GunPid2 = apns_connection:gun_pid(ConnectionName),
  true = (GunPid =/= GunPid2),

  GunPid2 ! {crash, ServerPid},
  wait_for(fun() ->
      apns_connection:gun_pid(ConnectionName) == GunPid2
    end, false),

  GunPid3 = apns_connection:gun_pid(ConnectionName),
  true = (GunPid2 =/= GunPid3),

  ok = close_connection(ConnectionName),
  ok = application:set_env(apns, backoff_ceiling, 10),
  [_] = meck:unload(),
  ok.

-spec gun_connection_killed(config()) -> ok.
gun_connection_killed(_Config) ->
  ok = mock_gun_open(),
  ConnectionName = ?FUNCTION_NAME,
  {ok, _ServerPid}  = apns:connect(cert, ConnectionName),
  GunPid = apns_connection:gun_pid(ConnectionName),
  true = is_process_alive(GunPid),
  exit(GunPid, kill),
  wait_for(fun() -> is_process_alive(GunPid) end, false),
  wait_for(fun() ->
      apns_connection:gun_pid(ConnectionName) == GunPid
    end, false),
  GunPid2 = apns_connection:gun_pid(ConnectionName),
  true = (GunPid =/= GunPid2),
  ok = close_connection(ConnectionName),
  [_] = meck:unload(),
  ok.

-spec push_notification(config()) -> ok.
push_notification(_Config) ->
  ok = mock_gun_open(),
  ConnectionName = ?FUNCTION_NAME,
  {ok, ServerPid} = apns:connect(cert, ConnectionName),
  Headers = #{ apns_id          => <<"apnsid">>
             , apns_expiration  => <<"0">>
             , apns_priority    => <<"10">>
             , apns_topic       => <<"net.inaka.myapp">>
             },
  Notification = #{<<"aps">> => #{<<"alert">> => <<"you have a message">>}},
  DeviceId = <<"device_id">>,
  ok = mock_gun_cancel(),
  ResponseCode = 200,
  ResponseHeaders = [{<<"apns-id">>, <<"apnsid">>}],
  ok = mock_gun_post_responding({fin, ResponseCode, ResponseHeaders}),
  {ResponseCode, ResponseHeaders, no_body} =
    apns:push_notification(ConnectionName, DeviceId, Notification, Headers),

  %% Now mock an error from APNs
  ErrorCode = 400,
  ErrorHeaders = [{<<"apns-id">>, <<"apnsid2">>}],
  ErrorBody = <<"{\"reason\":\"BadTopic\"}">>,
  ok = mock_gun_post_responding({nofin, ErrorCode, ErrorHeaders, ErrorBody}),

  {ErrorCode, ErrorHeaders, ErrorBody} =
    apns:push_notification(ConnectionName, DeviceId, Notification),
  {ErrorCode, ErrorHeaders, ErrorBody} =
    apns:push_notification(ServerPid, DeviceId, Notification),

  ok = close_connection(ConnectionName),
  _ = meck:unload(),
  ok.

-spec push_notification_token(config()) -> ok.
push_notification_token(_Config) ->
  ok = mock_gun_open(),
  ConnectionName = ?FUNCTION_NAME,
  {ok, ServerPid} = apns:connect(token, ConnectionName),
  Headers = #{ apns_id          => <<"apnsid2">>
             , apns_expiration  => <<"0">>
             , apns_priority    => <<"10">>
             , apns_topic       => <<"net.inaka.myapp">>
             },
  Notification = #{<<"aps">> => #{<<"alert">> => <<"more messages">>}},
  DeviceId = <<"device_id2">>,

  ok = maybe_mock_apns_os(),
  Token = apns:generate_token(<<"TeamId">>, <<"KeyId">>),

  ok = mock_gun_cancel(),
  ResponseCode = 200,
  ResponseHeaders = [{<<"apns-id">>, <<"apnsid2">>}],
  ok = mock_gun_post_responding({fin, ResponseCode, ResponseHeaders}),
  {ResponseCode, ResponseHeaders, no_body} =
    apns:push_notification_token( ConnectionName
                                , Token
                                , DeviceId
                                , Notification
                                , Headers
                                ),
  {ResponseCode, ResponseHeaders, no_body} =
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

-spec push_notification_json_body(config()) -> ok.
push_notification_json_body(_Config) ->
  ok = mock_gun_open(),
  ConnectionName = ?FUNCTION_NAME,
  {ok, _ServerPid} = apns:connect(cert, ConnectionName),
  ok = mock_gun_cancel(),
  ok = mock_gun_post_capturing(self()),
  Headers = #{apns_topic => <<"net.inaka.myapp">>},
  DeviceId = <<"device_id">>,

  BinaryKeys = #{<<"aps">> => #{ <<"alert">> => <<"you have a message">>
                               , <<"badge">> => 3
                               }},
  {200, [], no_body} =
    apns:push_notification(ConnectionName, DeviceId, BinaryKeys, Headers),
  BinaryKeysBody = pushed_body(),
  true = is_binary(BinaryKeysBody),
  BinaryKeys = json:decode(BinaryKeysBody),

  AtomKeys = #{aps => #{alert => <<"hi">>, 'content-available' => 1}},
  {200, [], no_body} =
    apns:push_notification(ConnectionName, DeviceId, AtomKeys, Headers),
  #{<<"aps">> := #{ <<"alert">>             := <<"hi">>
                  , <<"content-available">> := 1
                  }} = json:decode(pushed_body()),

  ok = close_connection(ConnectionName),
  _ = meck:unload(),
  ok.

-spec generate_token_jwt_claims(config()) -> ok.
generate_token_jwt_claims(_Config) ->
  ok = maybe_mock_apns_os(),
  Token = apns:generate_token(<<"THEATEAM">>, <<"KEYID12345">>),
  [Header, Payload, Signature] = binary:split(Token, <<".">>, [global]),
  #{ <<"alg">> := <<"ES256">>
   , <<"typ">> := <<"JWT">>
   , <<"kid">> := <<"KEYID12345">>
   } = json:decode(base64url:decode(Header)),
  #{ <<"iss">> := <<"THEATEAM">>
   , <<"iat">> := Iat
   } = json:decode(base64url:decode(Payload)),
  true = is_integer(Iat),
  true = byte_size(Signature) > 0,
  _ = meck:unload(),
  ok.

-spec push_notification_from_any_process(config()) -> ok.
push_notification_from_any_process(_Config) ->
  ok = mock_gun_open(),
  ok = mock_gun_cancel(),
  ok = mock_gun_post_responding({fin, 200, []}),
  Self = self(),

  SpawnedPid = spawn(fun() ->
    {ok, Pid} = apns:connect(cert, undefined),
    Self ! {self(), Pid}
  end),

  ConnectionPid = receive
    {SpawnedPid, ServerPid} -> ServerPid
  after 5000 ->
    ct:fail(timeout_waiting_for_connection)
  end,

  Notification = #{<<"aps">> => #{<<"alert">> => <<"from another process">>}},
  {200, [], no_body} =
    apns:push_notification(ConnectionPid, <<"device_id">>, Notification, #{}),
  {200, [], no_body} =
    apns:push_notification_token( ConnectionPid
                                , <<"token">>
                                , <<"device_id">>
                                , Notification
                                , #{}
                                ),

  ok = close_connection(ConnectionPid),
  _ = meck:unload(),
  ok.

-spec push_notification_timeout(config()) -> ok.
push_notification_timeout(_Config) ->
  %% Change the timeout variable
  {ok, OriginalTimeout} = application:get_env(apns, timeout),
  ok = application:set_env(apns, timeout, 0),

  ok = mock_gun_open(),
  ok = mock_gun_post(),
  ok = mock_gun_cancel(),
  ConnectionName = ?FUNCTION_NAME,
  {ok, _ApnsPid} = apns:connect(cert, ConnectionName),
  Notification = #{<<"aps">> => #{<<"alert">> => <<"another message">>}},
  DeviceId = <<"device_id">>,
  {error, timeout} = apns:push_notification(ConnectionName, DeviceId, Notification),
  ok = close_connection(ConnectionName),
  _ = meck:unload(),

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
  {ok, OriginalApnsPushType} = application:get_env(apns, apns_push_type),

  ApnsId = "this is the ID",
  ApnsExp = 10,
  ApnsPriority = undefined,
  ApnsTopic = <<"com.example.mycoolapp">>,
  ApnsCollapseId = undefined,
  ApnsPushType = <<"background">>,

  ok = application:set_env(apns, apns_id, ApnsId),
  ok = application:set_env(apns, apns_expiration, ApnsExp),
  ok = application:set_env(apns, apns_priority, ApnsPriority),
  ok = application:set_env(apns, apns_topic, ApnsTopic),
  ok = application:set_env(apns, apns_collapse_id, ApnsCollapseId),
  ok = application:set_env(apns, apns_push_type, ApnsPushType),

  Expected = #{ apns_id => list_to_binary(ApnsId)
              , apns_expiration => list_to_binary(integer_to_list(ApnsExp))
              , apns_topic => ApnsTopic
              , apns_push_type => ApnsPushType
              },

  Expected = apns:default_headers(),

  %% The defaults always include apns_push_type regardless of environment:
  application:unset_env(apns, apns_push_type),
  #{apns_push_type := <<"alert">>} = apns:default_headers(),

  % turn back the original values
  ok = application:set_env(apns, apns_id, OriginalApnsId),
  ok = application:set_env(apns, apns_expiration, OriginalApnsExp),
  ok = application:set_env(apns, apns_priority, OriginalApnsPriority),
  ok = application:set_env(apns, apns_topic, OriginalApnsTopic),
  ok = application:set_env(apns, apns_collapse_id, OriginalApnsCollapseId),
  ok = application:set_env(apns, apns_push_type, OriginalApnsPushType),
  ok.

-spec test_coverage(config()) -> ok.
test_coverage(_Config) ->
  ok = mock_gun_open(),
  ConnectionName = ?FUNCTION_NAME,
  {ok, _ServerPid}  = apns:connect(cert, ConnectionName),

  {error, bad_call} = gen_server:call(ConnectionName, hello_call),
  ok = gen_server:cast(ConnectionName, hello_cast),
  ConnectionName ! hello_info,
  {ok, connected, #{}} = apns_connection:code_change(old_version, connected, #{}, []),

  ok = close_connection(ConnectionName),
  [_] = meck:unload(),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec wait_for(fun(() -> term()), term()) -> ok.
wait_for(Fun, Expected) ->
  wait_for(Fun, Expected, 50).

-spec wait_for(fun(() -> term()), term(), non_neg_integer()) -> ok.
wait_for(_Fun, Expected, 0) ->
  ct:fail({timeout_waiting_for, Expected});
wait_for(Fun, Expected, Retries) ->
  case Fun() of
    Expected ->
      ok;
    _ ->
      ok = timer:sleep(100),
      wait_for(Fun, Expected, Retries - 1)
  end.

-spec test_function() -> ok.
test_function() ->
  receive
    normal           -> ok;
    {crash, Pid}     -> Pid ! {gun_down, self(), http2, closed, [], []};
    _                -> test_function()
  after 60000 ->
    test_function()
  end.

-spec mock_gun_open() -> ok.
mock_gun_open() ->
  meck:expect(gun, open, fun(_, _, _) ->
    GunPid = spawn(fun test_function/0),
    self() ! {gun_up, GunPid, http2},
    {ok, GunPid}
  end).

-spec mock_gun_post() -> ok.
mock_gun_post() ->
  meck:expect(gun, post, fun(_, _, _, _) ->
    make_ref()
  end).

-spec mock_gun_post_capturing(pid()) -> ok.
mock_gun_post_capturing(TestPid) ->
  meck:expect(gun, post, fun(GunPid, _Path, _Headers, Body) ->
    StreamRef = make_ref(),
    TestPid ! {pushed_body, Body},
    self() ! {gun_response, GunPid, StreamRef, fin, 200, []},
    StreamRef
  end).

-spec mock_gun_cancel() -> ok.
mock_gun_cancel() ->
  meck:expect(gun, cancel, fun(_, _) -> ok end).

-spec pushed_body() -> binary().
pushed_body() ->
  receive
    {pushed_body, Body} -> Body
  after 1000 ->
    ct:fail("gun:post/4 was never called")
  end.

-spec mock_gun_post_responding({fin, non_neg_integer(), [term()]}
                              | {nofin, non_neg_integer(), [term()], binary()}) -> ok.
mock_gun_post_responding({fin, Status, Headers}) ->
  meck:expect(gun, post, fun(GunPid, _Path, _Headers, _Body) ->
    StreamRef = make_ref(),
    self() ! {gun_response, GunPid, StreamRef, fin, Status, Headers},
    StreamRef
  end);
mock_gun_post_responding({nofin, Status, Headers, Body}) ->
  meck:expect(gun, post, fun(GunPid, _Path, _Headers, _Body) ->
    StreamRef = make_ref(),
    self() ! {gun_response, GunPid, StreamRef, nofin, Status, Headers},
    self() ! {gun_data, GunPid, StreamRef, fin, Body},
    StreamRef
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
  wait_for(fun() ->
      is_process_alive(ConnectionId)
    end, false),
  ok.
