-module(apns_SUITE).
-author('Brujo Benavides <elbrujohalcon@inaka.net>').

-include("apns.hrl").
-include("localized.hrl").
-include_lib("common_test/include/ct.hrl").
-define(DEVICE_TOKEN,
        "139D3CAB173FB230B97E4A19D288E3FBCD4B037F9B18ABA17FE4CDE72085E994").

-define(TEST_CONNECTION, 'test-connection').

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([reconnect/1, dont_reconnect/1, minimal/1]).

-spec all() -> [atom()].
all() -> [minimal, reconnect, dont_reconnect].

-spec init_per_testcase(_, Config) -> Config.
init_per_testcase(_, Config) ->
  given_mock_apn(Config),
  given_ssl_config(Config),
  ok = apns:start(),
  Config.

-spec end_per_testcase(_, Config) -> Config.
end_per_testcase(dont_reconnect, Config) ->
  _ = apns:stop(),
  Config;
end_per_testcase(_, Config) ->
  _ = application:stop(mock_apn),
  end_per_testcase(dont_reconnect, Config).

%%% Tests
-spec dont_reconnect(_) -> {comment, []}.
dont_reconnect(_Config) ->
  Now = lists:flatten(io_lib:format("~p", [calendar:local_time()])),
  {ok, Pid} =
    apns:connect(dont_reconnect, fun log_error/2, fun log_feedback/1),
  Ref = erlang:monitor(process, Pid),

  ct:comment("APNS disconnects..."),
  application:stop(mock_apn),
  monitor_process_for_a_second(Ref),

  ct:comment("Connection should crash on next message"),
  ok =
    apns:send_message(
      dont_reconnect, ?DEVICE_TOKEN,
      Now ++ " - Test Alert", random:uniform(10), "chime"),
  {error, econnrefused} = wait_for_down_for_a_second(Ref),

  {comment, ""}.

%%% Tests
-spec reconnect(_) -> {comment, []}.
reconnect(_Config) ->
  Now = lists:flatten(io_lib:format("~p", [calendar:local_time()])),
  {ok, Pid} =
    apns:connect(reconnect, fun log_error/2, fun log_feedback/1),
  Ref = erlang:monitor(process, Pid),

  ct:comment("A disconnection is simulated..."),
  mock_disconnection(Pid),
  monitor_process_for_a_second(Ref),

  ct:comment("New message should reconnect"),
  ok =
    apns:send_message(
      reconnect, ?DEVICE_TOKEN,
      Now ++ " - Test Alert", random:uniform(10), "chime"),
  monitor_process_for_a_second(Ref),

  {comment, ""}.

-spec minimal(_) -> {comment, []}.
minimal(_Config) ->
  Now = lists:flatten(io_lib:format("~p", [calendar:local_time()])),
  {ok, Pid} =
    apns:connect(minimal, fun log_error/2, fun log_feedback/1),
  Ref = erlang:monitor(process, Pid),
  ok =
    apns:send_message(
      minimal, ?DEVICE_TOKEN,
      Now ++ " - Test Alert", random:uniform(10), "chime"),
  monitor_process_for_a_second(Ref),

  ok =
    apns:send_message(
      minimal,
      ?DEVICE_TOKEN,
      #loc_alert{ action = "ACTION",
                  args   = ["arg1", "arg2"],
                  body   = Now ++ " - Localized Body",
                  image  = none,
                  key    = "KEY"},
      random:uniform(10),
      "chime"),
  monitor_process_for_a_second(Ref),

  ok =
    apns:send_message(
      minimal, ?DEVICE_TOKEN, #loc_alert{key = "EMPTY"},
      random:uniform(10), "chime"),
  monitor_process_for_a_second(Ref),

  ok =
    apns:send_message(
      minimal, ?DEVICE_TOKEN, Now ++ " - Test Alert",
      random:uniform(10), "chime",
      apns:expiry(86400), [{<<"acme1">>, 1}]),
  monitor_process_for_a_second(Ref),

  ok =
    apns:send_message(
      minimal, ?DEVICE_TOKEN,
      #loc_alert{ action = "ACTION",
                  args   = ["arg1", "arg2"],
                  body   = Now ++ " - Localized Body",
                  image  = none,
                  key    = "KEY"},
      random:uniform(10), "chime",
      apns:expiry(86400),
      [ {<<"acme2">>, <<"x">>},
        {<<"acme3">>, {[{<<"acme4">>, false}]}}]),
  monitor_process_for_a_second(Ref),

  ok = apns:send_message(minimal, #apns_msg{device_token = ?DEVICE_TOKEN,
                                 sound = "chime",
                                 badge = 12,
                                 expiry = apns:expiry(86400),
                                 alert = "Low Priority alert",
                                 priority = 0}),
  monitor_process_for_a_second(Ref).

log_error(MsgId, Status) ->
  error_logger:error_msg("Error on msg ~p: ~p~n", [MsgId, Status]).

log_feedback(Token) ->
  error_logger:warning_msg("Device with token ~p removed the app~n", [Token]).

monitor_process_for_a_second(Ref) ->
  receive
    {'DOWN', Ref, _, _, _} = DownMsg ->
      throw(DownMsg);
    UnexpectedMessage ->
      throw(UnexpectedMessage)
  after 1000 ->
    ok
  end.

wait_for_down_for_a_second(Ref) ->
  receive
    {'DOWN', Ref, _, _, Reason} ->
      Reason;
    UnexpectedMessage ->
      throw(UnexpectedMessage)
  after 1000 ->
    ct:fail("~p should've died", [Ref])
  end.

given_mock_apn(Config) ->
  % applications are loaded during start, which overwrites env variables
  % but if you first load the app, it is not loaded second time during app start
  application:load(mock_apn),
  application:set_env(
    mock_apn, certfile, ?config(data_dir, Config) ++ "cert.pem"),
  application:set_env(
    mock_apn, keyfile, ?config(data_dir, Config) ++ "key.pem"),
  application:ensure_started(mock_apn).

given_ssl_config(Config) ->
  application:load(apns),
  application:set_env(apns, cert_file, ?config(data_dir, Config) ++ "cert.pem"),
  application:set_env(apns, key_file, ?config(data_dir, Config) ++ "key.pem").

mock_disconnection(Pid) ->
  State = sys:get_state(Pid),
  [state, Socket | _] = tuple_to_list(State),
  Pid ! {ssl_closed, Socket}.
