%% @doc Connection Parameters
-record(apns_connection, {ssl_seed    = "someseedstring"                  :: string(),
                          apple_host  = "gateway.sandbox.push.apple.com"  :: string(),
                          apple_port  = 2195                              :: integer(),
                          cert_file   = "priv/cert.pem"                   :: string(),
                          timeout     = 30000                             :: integer()
                          }).
-record(apns_msg, {device_token :: string(),
                   alert = none :: none | string(),
                   badge = none :: none | integer(),
                   sound = none :: none | string()}).