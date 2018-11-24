
Apns Erlang
=======

<img src="https://media.giphy.com/media/uZQP0PR0BmkGA/giphy.gif" align="right" style="float:right" height="400" />

This lib is intended to allow you to write an APNs provider for [Apple Push Notificaion services (APNs)](https://developer.apple.com/library/content/documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/APNSOverview.html) over HTTP2 in Erlang.

Copyright (c) 2017 Erlang Solutions Ltd. <support@inaka.net>, released under the Apache 2 license

__Note:__ Currently it supports push notifications with certificate and authentication token.

__BINARY_API:__  This lib supports http2. Check [using Legacy Binary API](https://github.com/softwarejoint/ex_apns)

## Requirements
- You must have installed an updated Openssl version or, at least, be sure it supports TLS 1.2+. New APNs server only supports connections over TLS 1.2+.
- Erlang R21

### How to compile:

`apns_erlang` user `erlang.mk` as make system. To compile

    $ make
    
To generate release

    $ make rel
    
### How to use with rebar:

You can use `apns_erlang` as a dependency in your rebar.config:

    {deps , [
        {apns, ".*", {git, "https://github.com/softwarejoint/apns_erlang", {tag, "2.4.0"}}}
    ]}.

### How to run the application apns_erlang:

1. copy the conf/sys.config.example to conf/sys.config
2. Setup the necessary values
3. `make rel` will create a release under `_rel/apns` directory. 

```
$ cd _rel/apns
$ bin/apns console
```
    
## How to configure it?

Checkout the sample config file in config/ directory.

You can find more info [here](https://developer.apple.com/library/content/documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/CommunicatingwithAPNs.html)

The other way is send all that info as a parameter to `apns:connect/1` function encapsulated in a `apns_connection:connection()` structure:

```erlang
#{ name       			:= name()
 , env        			:= 'development' | 'production'
 `Refer Connection Options`
 `Refer Other Options`
 `Refer Feedback Option`
 }.
```

- `name` is specified, the connection process is registered by `name`.

- `env` defines which apple host to connect to. Default `env` is `production`


#### `Connection Options` for `Provider Authentication Tokens`

```erlang
#{ token_keyfile   		=> path()
 , token_kid   			=> binary()
 , team_id			=> binary()
 }.
```

- `token_keyfile` is a path to the Authentication Key provided by Apple. This is in `.p8` format and it doesn't need conversion.

- `token_kid` is the token key file id when generated. It is the last string after `_` in auth `token_keyfile` file name. Ex: `APNsAuthKey_KEYID12345.p8`, `token_kid` is `KEYID12345`.

- `team_id` is Apple developer TEAM id as on apple developer console.

Library maintains token signing and hourly renewal of signature key.

#### `Connection Options` for `Provider Certificate`

```erlang
#{ certfile   			=> path()
 , keyfile			=> path()
 }.
```

Supply cert paths in `certfile` and `keyfile`. 

We need `certfile` and `keyfile` in `.pem` format, here is an example of how to convert them, check the [certificates](https://blog.serverdensity.com/how-to-build-an-apple-push-notification-provider-server-tutorial/) section.

Alternatively, you can supply a cert binary in `certdata` and a `keydata`

```erlang
#{ certdata   			=> binary()
 , keydata			=> keydata()
 }.
```

#### `Other Options`

In above connection option following config variables can be specified.

```erlang
#{ apple_port 			=> integer()
 , timeout			=> integer()
 `Refer Reconnection Option`
 `Refer Header Options`
  }.
```

- `apple_port` https port to use. Default is 443.
- `timeout` http body receive timeout in milliseconds. Default is 5000.

#### `Feedback Option`

```erlang
#{ feedback   			=> { Module, Function } }.
```

Only invalid token causes the callback function to be called.
Feedback function can be specified in the sys.config file.

It takes the following form.

```erlang
M:F(ConnectionName, ApnsId | DeviceId, InValidationTimestamp).
```

- `ConnectionName` is apns connection registeration name.
	
- `ApnsId` is apns_id specified in headers. `DeviceId` is apple token. While calling `push_notification/4` if headers contain `apns_id` then callback will contain the same else library maintains `apns_id` to `DeviceId` and returns the same in callback.
	
- `InValidationTimestamp` in UTC seconds when apple device token was last valid.
	
## Create connections

After defining config options as above.

Example:

```erlang
1> apns:connect(cert, my_first_connection).
{ok,<0.87.0>}
2> apns:connect(#{name => another_cert, env => production, 
     certfile => "priv/cert.pem", keyfile => "priv/key.pem"}).
3> apns:connect(token, my_second_connection).
{ok,<0.95.0>}
```

`apns:connect/2` returns the connection `pid`.

## Create Connections without name

In some scenarios we don't want to assign names to the connections instead we want working just with the `pid` (working with a pool of connections for example). If that is the case we use the same `apns:connect/1` and `apns:connect/2` functions but instead of a connection name we put `undefined`:

```erlang
1> apns:connect(cert, undefined).
{ok,<0.127.0>}
2> apns:connect(#{name => undefined, env => production,
certfile => "priv/cert2.pem", keyfile => "priv/key2-noenc.pem"}).
{ok,<0.130.0>}
3> apns:connect(token, my_second_connection).
{ok,<0.132.0>}
```

## Push Notifications

We will use `apns:push_notification/3,4`.

We will need the connection, a notification, the device ID and some http2 headers. The connection is the `atom` we used when we executed `apns:connect/2` for setting a name or its `pid`, the device ID is provided by Apple, the notification is a `map` with the data we want to send, that map will be encoded to json later and the http2 headers can be explicitly sent as a parameter using `apns:push_notification/4` or can be defined at the `config` file, in that case we would use `apns:push_notification/3`.

This is the `headers` format:	**_`Header Options`_**


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

- apns_id can be generated by `apns_connection:new_apns_id()`

Lets send a Notification.

```erlang
1> {ok, Pid} = apns:connect(cert, my_first_connection).
{ok,<0.85.0>}
2> DeviceId = <<"a0dc63fb059cb9c13b03e5c974af3dd33d67fed4147da8c5ada0626439e18935">>.
<<"a0dc63fb059cb9c13b03e5c974af3dd33d67fed4147da8c5ada0626439e18935">>
3> Notification = #{aps => #{alert => <<"you have a message">>}}.
#{aps => #{alert => <<"you have a message">>}}
4> apns:push_notification(my_first_connection, DeviceId, Notification).
ok
5> apns:push_notification(Pid, DeviceId, Notification).
ok
```

This library works asynchronously. See `Feedback Option` for feedback.
And that's all.

## `Reconnection Option`

If network goes down or something unexpected happens the `gun` connection with APNs will go down. In that case `apns_erlang` will send a message `{reconnecting, ServerPid}` to the client process, that means `apns_erlang` lost the connection and it is trying to reconnect. Once the connection has been recover a `{connection_up, ServerPid}` message will be send.

We implemented an *Exponential Backoff* strategy. We can set the *ceiling* time adding the `backoff_ceiling` variable on the `config` file. By default it is set to 10 (seconds).

```erlang
#{ 
    backoff_ceiling		=> integer()
 }.
``` 

- `backoff_ceiling` backoff duration for reconnection in seconds. Default is 10.

## Close connections

Apple recommends us to keep our connections open and avoid opening and closing very often. You can check the [Best Practices for Managing Connections](https://developer.apple.com/library/content/documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/CommunicatingwithAPNs.html) section.

But when closing a connection makes sense `apns_erlang` gives us the function `apns:close_connection/1` where the parameter is the connection's name or the connection's `pid`. After using it the name will be available for new connections again (if it was different than `undefined`).

