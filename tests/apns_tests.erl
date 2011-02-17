-module(apns_tests).

-include("apns.hrl").
-include("localized.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("eunit.hrl").
%-define(DEVICE_TOKEN, "AC812B2D723F40F206204402F1C870C8D8587799370BD41D6723145C4E4EBBD7").
-define(DEVICE_TOKEN, "139D3CAB173FB230B97E4A19D288E3FBCD4B037F9B18ABA17FE4CDE72085E994").

-define(TEST_CONNECTION, 'test-connection').

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
  end,
  ?assertEqual(ok, apns:send_message(?TEST_CONNECTION, ?DEVICE_TOKEN, "Test Alert", random:uniform(10), "chime", [{<<"acme1">>, 1}])),
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
                                     random:uniform(10), "chime", [{<<"acme2">>, <<"x">>},
                                                                   {<<"acme3">>, {[{<<"acme4">>, false}]}}])),
  receive
    {'DOWN', Ref, _, _, _} = DownMsg5 ->
      ?fail(DownMsg5);
    DownMsg5 ->
      ?fail(DownMsg5)
    after 1000 ->
      ok
  end.