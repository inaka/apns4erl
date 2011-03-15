%% Connection Parameters
-record(apns_connection, {ssl_seed          = "someseedstring"                  :: string(),
                          apple_host        = "gateway.sandbox.push.apple.com"  :: string(),
                          apple_port        = 2195                              :: integer(),
                          cert_file         = "priv/cert.pem"                   :: string(),
                          timeout           = 30000                             :: integer(),
                          feedback_host     = "feedback.sandbox.push.apple.com" :: string(),
                          feedback_port     = 2196                              :: integer(),
                          feedback_fun      = fun erlang:display/1              :: fun((string()) -> _),
                          feedback_timeout  = 30*60*1000                        :: pos_integer()
                          }).
-record(apns_msg, {device_token :: string(),
                   alert = none :: none | apns:alert(),
                   badge = none :: none | integer(),
                   sound = none :: none | string(),
                   extra = []   :: [apns_mochijson2:json_property()]}).