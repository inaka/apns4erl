-module(apns_tests).

-include("apns.hrl").
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
  ?assertEqual(ok, apns:send_badge(?TEST_CONNECTION, ?DEVICE_TOKEN, 1)),
  receive
    {'DOWN', Ref, _, _, _} = DownMsg ->
      ?fail(DownMsg);
    DownMsg ->
      ?fail(DownMsg)
    after 1000 ->
      ok
  end.