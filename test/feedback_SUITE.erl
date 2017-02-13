-module(feedback_SUITE).
-author("Felipe Ripoll <ferigis@inakanetworks.com>").

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        ]).

-export([ get_feedback/1
        , get_feedback_timeout/1
        , test_coverage/1
        ]).

-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() -> [atom()].
all() ->  [ get_feedback
          , get_feedback_timeout
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

-spec get_feedback(config()) -> ok.
get_feedback(_Config) ->
  ok = meck:expect(ssl, connect, fun(_, _, _, _) ->
    {ok, my_socket}
  end),
  ok = meck:expect(ssl, close, fun(my_socket) ->
    ok
  end),
  Timestamp = 1486407930,
  Device = 16#A0DC63FB059CB9C13B03E5C974AF3DD33D67FED4147DA8C5ADA0626439E18935,
  DeviceEncoded = binary:encode_unsigned(Device),
  Length = size(DeviceEncoded),
  Message1 = << Timestamp:4/unit:8
              , Length:2/unit:8
              , DeviceEncoded/binary
              , Timestamp:4/unit:8 >>,
  Message2 = << Length:2/unit:8 >>,
  Message3 = << DeviceEncoded/binary>>,
  self() ! {ssl, my_socket, Message1},
  self() ! {ssl, my_socket, Message2},
  self() ! {ssl, my_socket, Message3},
  Feedback =
    { {{2017, 2, 6}, {19, 5, 30}}
    , "A0DC63FB059CB9C13B03E5C974AF3DD33D67FED4147DA8C5ADA0626439E18935"
    },
  % We sent the same feedback twice
  [Feedback, Feedback] = apns:get_feedback(),
  [_] = meck:unload(),
  ok.

-spec get_feedback_timeout(config()) -> ok.
get_feedback_timeout(_Config) ->
  ok = meck:expect(ssl, connect, fun(_, _, _, _) ->
    {ok, my_socket1}
  end),
  ok = meck:expect(ssl, close, fun(my_socket1) ->
    ok
  end),
  %% Change the timeout variable
  {ok, OriginalTimeout} = application:get_env(apns, timeout),
  ok = application:set_env(apns, timeout, 0),

  timeout = apns:get_feedback(),

  % turn back the original values
  ok = application:set_env(apns, timeout, OriginalTimeout),
  [_] = meck:unload(),
  ok.

-spec test_coverage(config()) -> ok.
test_coverage(_Config) ->
  ok = meck:expect(ssl, connect, fun(_, _, _, _) ->
    {error, error_reason}
  end),

  {error, error_reason} = apns:get_feedback(),

  {ok, KeyFile} = application:get_env(apns, keyfile),
  application:unset_env(apns, keyfile),

  {error, error_reason} = apns:get_feedback(),

  % turn back the original values
  ok = application:set_env(apns, keyfile, KeyFile),
  [_] = meck:unload(),
  ok.
