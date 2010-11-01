-define(assertLess(Min, Max),
  ((fun (__X, __V) ->
             case (__X < __V) of
               true -> ok;
               false -> .erlang:error({assertLess_failed,
                                     [{module, ?MODULE},
                                      {line, ?LINE},
                                      {expression, (??Min " < " ??Max)},
                                      {expected_min, __X},
                                      {actual_min, __V}]})
             end
    end)(Min, Max))).
-define(assertLessOrEq(Min, Max),
  ((fun (__X, __V) ->
             case (__X =< __V) of
               true -> ok;
               false -> .erlang:error({assertLessOrEq_failed,
                                     [{module, ?MODULE},
                                      {line, ?LINE},
                                      {expression, (??Min " =< " ??Max)},
                                      {expected_min, __X},
                                      {actual_min, __V}]})
             end
    end)(Min, Max))).
-define(assertMember(Member, List),
  ((fun (__X, __V) ->
             case lists:member(__X, __V) of
               true -> ok;
               false -> .erlang:error({assertMember_failed,
                                     [{module, ?MODULE},
                                      {line, ?LINE},
                                      {expression, ("lists:member(" ??Member " , " ??List ")")},
                                      {element, __X},
                                      {list, __V}]})
             end
    end)(Member, List))).
-define(assertNotMember(Member, List),
  ((fun (__X, __V) ->
             case lists:member(__X, __V) of
               false -> ok;
               true -> .erlang:error({assertNotMember_failed,
                                     [{module, ?MODULE},
                                      {line, ?LINE},
                                      {expression, ("not lists:member(" ??Member " , " ??List ")")},
                                      {element, __X},
                                      {list, __V}]})
             end
    end)(Member, List))).
-define(fail(Msg),
        .erlang:error({failed,
                       [{module, ?MODULE},
                       {line, ?LINE},
                       {msg, Msg}]})).

-define(USERS, 7500).
-define(FRIENDS, 100).
-define(MSGS, 15000).
-define(CLIENT_HOST, {127,0,0,1}).
-define(CLIENT_PORT, 8003).
-define(FAKE_SHOW, 666).
