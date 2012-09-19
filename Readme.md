Apns4erl
========

This lib is intended to allow you to write an APNs provider for Apple Push Notificaion services (APNs) in Erlang.

Copyright (c) 2010 Inaka Labs SRL <support@inaka.net>, released under the MIT license

Example
=======

Using apns4erl is quite simple. First, setup something similar to this in your sys.config:

```erlang
    {apns, [
      {apple_host, "gateway.sandbox.push.apple.com"},
      {apple_port, 2195},
      {cert_file, "/etc/certs/mycert_dev.pem"},
      {key_file, undefined},
      {cert_password, undefined},
      {timeout, 30000},
      {feedback_port, 2196},
      {feedback_host, "feedback.sandbox.push.apple.com"},
      {feedback_timeout, 18000000}
    ]}
```

**NOTE**: The *apple_host* to use will depend on your environment (production or development). Remember to always use the **correct** certificate, device tokens, and apns hostname for production or development environments.

**NOTE 2**: To generate the .pem file, from the .cer an .p12 files provided by Apple, you can use [this script](/inaka/apns4erl/blob/master/priv/test_certs)

Then, once you've started the apns application, you can connect to the APNS network using:

```erlang
      apns:connect(
        my_connection_name,                   % you connection identifier
        fun handle_apns_error/2,              % called in case of a "hard" error
        fun handle_apns_delete_subscription/1 % called if the device uninstalled the application
      ).
```

As a result, you will get a tuple:

 * ``{ok, Pid}``
 * ``{error, {already_started, Pid}}``
 * ``{error, Reason}``

**Pid** is the Pid of the [apns_connection](/inaka/apns4erl/blob/master/src/apns_connection.erl) process spawned to handle the connection.

To send a notification
======================
    apns:send_message(my_connection_name, "this_is_a_valid_device_token", "hello world").

That's it!

A little more about what's going on
===================================
Actually, send\_message/3, send\_message/4, send\_message/5, send\_message/6, send\_message/7, and send\_message/8 are calling send\_message/2, which takes a **#apns\_msg** record as its 2nd argument. Thus, you can also create the message customized with your own needs, by using a **#apns\_msg** record:
```erlang
    -include_lib("apns/include/apns.hrl").

    apns:send_message(my_connection_name, #apns_msg{
      alert  = "alert" ,
      badge  = 1,
      sound  = "sound" ,
      expiry = 1348000749,
      device_token = "this_is_a_valid_device_token"
    }).
```

Feedback Channel and Getting Errors
===================================
Notice how we are passing 2 funs to the connect function. These are used as callbacks:

If there was an error while sending a message, the first fun will be called.

If there were no errors, but Apple reported that the user removed the application from the device, the 2nd fun will be used (this is effectively the feedback channel).

```erlang
    handle_apns_error(MsgId, Status) ->
      error_logger:error_msg("error: ~p - ~p~n", [MsgId, Status]).

    handle_apns_delete_subscription(Data) ->
      error_logger:info_msg("delete subscription: ~p~n", [Data]).
```