-module(apns_tests).

-include("apns.hrl").
-include("localized.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("eunit.hrl").
-define(DEVICE_TOKEN, "AC812B2D723F40F206204402F1C870C8D8587799370BD41D6723145C4E4EBBD7").
-define(TEST_CONNECTION, 'test-connection').

-spec test() -> any().
-spec apns_test_() -> {setup, fun(() -> ok), fun((_) -> ok), fun(() -> any())}.
apns_test_() ->
  {setup,
   fun() -> ok end,
   fun(_) ->
           ?assertEqual(ok, apns:disconnect(?TEST_CONNECTION)),
           ?assertEqual(ok, apns:stop())
   end,
   fun run/0
   }.

%%% Tests
run() ->
  ?assertEqual(ok, apns:start()),
  {ok, Pid} = apns:connect(?TEST_CONNECTION),
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
  end.