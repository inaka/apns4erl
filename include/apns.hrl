%% Connection Parameters

-record(apns_connection, {apple_host        = "gateway.sandbox.push.apple.com"      :: string(),
                          apple_port        = 2195                                  :: integer(),
                          cert_file         = "priv/cert.pem"                       :: string(),
                          key_file          = undefined                             :: undefined | string(),
						  cert_password     = undefined								:: undefined | string(),
                          timeout           = 30000                                 :: integer(),
                          error_fun         = fun(X,Y) -> erlang:display({X,Y}) end :: fun((binary(), apns:status()) -> stop | _),
                          feedback_host     = "feedback.sandbox.push.apple.com"     :: string(),
                          feedback_port     = 2196                                  :: integer(),
                          feedback_fun      = fun erlang:display/1                  :: fun(({calendar:datetime(), string()}) -> _),
                          feedback_timeout  = 30*60*1000                            :: pos_integer()
                          }).
-record(apns_msg, {id = apns:message_id()       :: binary(),
                   expiry = apns:expiry(86400)  :: non_neg_integer(), %% default = 1 day
                   device_token                 :: string(),
                   alert = none                 :: none | apns:alert(),
                   badge = none                 :: none | integer(),
                   sound = none                 :: none | apns:apns_str(),
                   apns_extra = []              :: none | [{atom(), integer()|boolean()|string()}],
                   extra = []                   :: [apns_mochijson2:json_property()]}).
