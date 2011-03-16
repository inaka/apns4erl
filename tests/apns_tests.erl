-module(apns_tests).

-include("apns.hrl").
-include("localized.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("eunit.hrl").
-define(DEVICE_TOKEN, "7F0CD8917A1D08BAE2F49666292E2DC7B8BDEA8D8C7673282E5CA0741D59F276").
%-define(DEVICE_TOKEN, "139D3CAB173FB230B97E4A19D288E3FBCD4B037F9B18ABA17FE4CDE72085E994"). %% Right for priv/cert.pem
%-define(DEVICE_TOKEN, "139D3CAB173AB230B97E4A19D288E3FBCD4B037F9B18ABA17FE4CDE72085E994"). %% Wrong

-define(TEST_CONNECTION, 'test-connection').

-export([main/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXTERNAL FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec main() -> no_return().
main() ->
  _ = application:load(apns),
  case eunit:test(apns, [verbose]) of
    ok -> halt(0);
    _ -> halt(1)
  end.

-spec test() -> any().
-spec apns_test_() -> {setup, fun(() -> ok), fun((_) -> ok), {timeout, 120000, fun(() -> any())}}.
apns_test_() ->
  {setup,
   fun() -> ok end,
   fun(_) ->
           ?assertEqual(ok, apns:disconnect(?TEST_CONNECTION)),
           ?assertEqual(ok, apns:stop())
   end,
   {timeout, 120000, fun run/0}
   }.

%%% Tests
run() ->
  ?assertEqual(ok, apns:start()),
  {ok, Pid} = apns:connect(?TEST_CONNECTION, fun log_error/2, fun log_feedback/1),
  Ref = erlang:monitor(process, Pid),
  ?assertEqual(ok, apns:send_message(?TEST_CONNECTION, ?DEVICE_TOKEN, "Test Alert", random:uniform(10), "chime")),
  receive
    {'DOWN', Ref, _, _, _} = DownMsg ->
      ?fail(DownMsg);
    DownMsg ->
      ?fail(DownMsg)
    after 1000 ->
      ok
  end,
  ?assertEqual(ok, apns:send_message(?TEST_CONNECTION, ?DEVICE_TOKEN, #loc_alert{action = "ACTION",
                                                                                 args   = ["arg1", "arg2"],
                                                                                 body   = "Localized Body",
                                                                                 image  = none,
                                                                                 key    = "KEY"},
                                     random:uniform(10), "chime")),
  receive
    {'DOWN', Ref, _, _, _} = DownMsg2 ->
      ?fail(DownMsg2);
    DownMsg2 ->
      ?fail(DownMsg2)
    after 1000 ->
      ok
  end,
  ?assertEqual(ok, apns:send_message(?TEST_CONNECTION, ?DEVICE_TOKEN, #loc_alert{key = "EMPTY"},
                                     random:uniform(10), "chime")),
  receive
    {'DOWN', Ref, _, _, _} = DownMsg3 ->
      ?fail(DownMsg3);
    DownMsg3 ->
      ?fail(DownMsg3)
    after 1000 ->
      ok
  end,
  ?assertEqual(ok, apns:send_message(?TEST_CONNECTION, ?DEVICE_TOKEN, "Test Alert",
                                     random:uniform(10), "chime",
                                     apns:expiry(120), [{<<"acme1">>, 1}])),
  receive
    {'DOWN', Ref, _, _, _} = DownMsg4 ->
      ?fail(DownMsg4);
    DownMsg4 ->
      ?fail(DownMsg4)
    after 1000 ->
      ok
  end,
  ?assertEqual(ok, apns:send_message(?TEST_CONNECTION, ?DEVICE_TOKEN, #loc_alert{action = "ACTION",
                                                                                 args   = ["arg1", "arg2"],
                                                                                 body   = "Localized Body",
                                                                                 image  = none,
                                                                                 key    = "KEY"},
                                     random:uniform(10), "chime",
                                     apns:expiry(120), [{<<"acme2">>, <<"x">>},
                                                        {<<"acme3">>, {[{<<"acme4">>, false}]}}])),
  receive
    {'DOWN', Ref, _, _, _} = DownMsg5 ->
      ?fail(DownMsg5);
    DownMsg5 ->
      ?fail(DownMsg5)
    after 1000 ->
      ok
  end.

log_error(MsgId, Status) ->
  error_logger:error_msg("Error on msg ~p: ~p~n", [MsgId, Status]).

log_feedback(Token) ->
  error_logger:warning_msg("Device with token ~p removed the app~n", [Token]).