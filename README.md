
Apns4erl v2
========

<img src="https://media.giphy.com/media/uZQP0PR0BmkGA/giphy.gif" align="right" style="float:right" height="400" />

This lib is intended to allow you to write an APNs provider for [Apple Push Notificaion services (APNs)](https://developer.apple.com/library/content/documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/APNSOverview.html) over HTTP2 in Erlang.

Copyright (c) 2017 Erlang Solutions Ltd. <support@inaka.net>, released under the Apache 2 license

You can find the v1 [here](https://github.com/inaka/apns4erl/releases/tag/1.0.6-final)?

__Note:__ `Apns4erl v2` is still under development. Currently it supports push notifications with certificate and authentication token.

Contact Us
==========
For **questions** or **general comments** regarding the use of Apns4erl, please use our public
[hipchat room](http://inaka.net/hipchat).

If you find any **bugs** or have a **problem** while using Apns4erl, please [open an issue](https://github.com/inaka/apns4erl/issues/new) in this repo (or a pull request :)).

And you can check all of our open-source projects at [inaka.github.io](http://inaka.github.io)

## Requirements
You must have installed an updated Openssl version or, at least, be sure it supports TLS 1.2+. New APNs server only supports connections over TLS 1.2+.

## How to use it?

First we have to fill our `config` file. This is an example you can find at `test/test.config`:

```
{
  apns,
  [ {apple_host,       "api.development.push.apple.com"}
  , {apple_port,       443}
  , {certfile,         "priv/apns-dev-cert.pem"}
  , {keyfile,          "priv/apns-dev-key-noenc.pem"}
  , {token_keyfile,    "priv/APNsAuthKey_KEYID12345.p8"}
  , {timeout,          10000}

  %% APNs Headers

  , {apns_id,          undefined}
  , {apns_expiration,  0}
  , {apns_priority,    10}
  , {apns_topic,       "com.example.myapp"}
  , {apns_collapse_id, undefined}
  ]
}
```

APNs allows two connection types, one is using `Provider Certificates`. If you want to use that way make sure you fill the `certfile` and `keyfile`. Those are paths to the `Provider Certificated` and the `Private Key` both provided by Apple. We need them in `.pem` format, here is an example of how to convert them, check the [certificates](https://blog.serverdensity.com/how-to-build-an-apple-push-notification-provider-server-tutorial/) section.

The other way to connect against APNs is using `Provider Authentication Tokens`, for this choice you must fill the field `token_keyfile`. This is a path to the Authentication Key provided by Apple. This is in `.p8` format and it doesn't need conversion.

This `key` will be needed in order to generate a token which will be used every time we try to push a notification. In connection's time it is not needed.

## Run

`apns4erl` can be included as a dependency and started from `yourapp.app.src`. You also can run it on the shell for testing.

```
> rebar3 compile
> erl -pa _build/default/lib/*/ebin -config test/test.config
```
Don't forget your config file.
```erlang
1> apns:start().
ok
```

## Create connections

After filling the `config` file and running `apns4erl` app we can start creating connections. As we mentioned before there are two types of connections. Both are created using the function `apns:connect/2` where the first argument is the type and the second one is the connection's name.

```erlang
2> apns:connect(cert, my_first_connection).
{ok,<0.87.0>}
3> apns:connect(token, my_second_connection).
{ok,<0.95.0>}
```
Note `cert` and `token` define the type we want.

Although `Apns4erl` is supervising the connections `apns:connect/2` returns the connection `pid` just in case you want to monitor it.

## Push Notifications over `Provider Certificate` connections

In order to send Notifications over `Provider Certificate` connection we will use `apns:push_notification/3,4`.

We will need the connection, a notification, the device ID and some http2 headers. The connection is the `atom` we used when we executed `apns:connect/2` for setting a name, the device ID is provided by Apple, the notification is a `map` with the data we want to send, that map will be encoded to json later and the http2 headers can be explicitly sent as a parameter using `apns:push_notification/4` or can be defined at the `config` file, in that case we would use `apns:push_notification/3`.

This is the `headers` format:

```erlang
-type headers()   :: #{ apns_id          => binary()
                      , apns_expiration  => binary()
                      , apns_priority    => binary()
                      , apns_topic       => binary()
                      , apns_collapse_id => binary()
                      , apns_auth_token  => binary()
                      }.
```

All of them are defined by Apple  [here](https://developer.apple.com/library/content/documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/CommunicatingwithAPNs.html)

Lets send a Notification.

```erlang
1> apns:connect(cert, my_first_connection).
{ok,<0.85.0>}
2> DeviceId = <<"a0dc63fb059cb9c13b03e5c974af3dd33d67fed4147da8c5ada0626439e18935">>.
<<"a0dc63fb059cb9c13b03e5c974af3dd33d67fed4147da8c5ada0626439e18935">>
3> Notification = #{aps => #{alert => <<"you have a message">>}}.
#{aps => #{alert => <<"you have a message">>}}
4> apns:push_notification(my_first_connection, DeviceId, Notification).
{200,
 [{<<"apns-id">>,<<"EFDE0D9D-F60C-30F4-3FF1-86F3B90BE434">>}],
 no_body}
```

The result is the response itself, its format is:

```erlang
-type response()  :: { integer()          % HTTP2 Code
                     , [term()]           % Response Headers
                     , [term()] | no_body % Response Body
                     } | timeout.
```

And that's all.

## Push Notifications over `Provider Authentication Tokens` connections

This is the other way APNs allows us to send notifications. In this case we don't need a certificate but we will need a `p8` file with the private key we will use to sign the token. Lets assume we've got the file  `APNsAuthKey_KEYID12345.p8` from Apple. We then have to fill the `config` file key `token_keyfile` with the path to that file.

We will need a `kid` value, this is the key identifier. In our case is the last 10 chars of the file name (`KEYID123456`). We will need also the `iss` value, this is the Team Id, that can be checked on your Apple's Developer account, in our case it will be `THEATEAM`. And that's it.

You can find more info [here](https://developer.apple.com/library/content/documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/CommunicatingwithAPNs.html)

In order to push a notification we will use `apns:push_notification_token/4,5`. We will need the same attributes we used sending a notification over `Provider Certificate` connections plus a signed `token`. This token has a 1 hour life, so that means we can generate one token and use it many times till it expires. Lets try.

Create the token:

```erlang
6> TeamId = <<"THEATEAM">>.
<<"THEATEAM">>
7> KeyID = <<"KEYID123456">>.
<<"KEYID123456">>
8> Token = apns:generate_token(TeamId, KeyID).
<<"eyJhbGciOiJFUzI1NiIsInR5cCI6IkpXVCIsImtpZCI6IktFWUlEMTIzNDU2In0.eyJpc3MiOiJUSEVBVEVBTSIsImlhdCI6MTQ4NjE0OTMzNH0.MEQC"...>>
```

Now push the notification:

```erlang
12> DeviceId = <<"a0dc63fb059cb9c13b03e5c974af3dd33d67fed4147da8c5ada0626439e18935">>.
<<"a0dc63fb059cb9c13b03e5c974af3dd33d67fed4147da8c5ada0626439e18935">>
13> Notification = #{aps => #{alert => <<"you have a message">>}}.
#{aps => #{alert => <<"you have a message">>}}
14> apns:push_notification_token(my_second_connection, Token, DeviceId, Notification).
{200,
 [{<<"apns-id">>,<<"EBC03BF9-A784-FDED-34F7-5A8D859DA977">>}],
 no_body}
```

We can use this token for an entire hour, after that we will receive something like this:

```erlang
16> apns:push_notification_token(my_second_connection, Token, DeviceId, Notification).
{403,
 [{<<"apns-id">>,<<"03FF9497-8A6B-FFD6-B32B-160ACEDE35F0">>}],
 [{<<"reason">>,<<"ExpiredProviderToken">>}]}
```

## Close connections

Apple recommends us to keep our connections open and avoid opening and closing very often. You can check the [Best Practices for Managing Connections](https://developer.apple.com/library/content/documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/CommunicatingwithAPNs.html) section.

But when closing a connection makes sense `apns4erl` gives us the function `apns:close_connection/1` where the parameter is the connection's name. After using it the name will be available for new connections again.
