# apns_pool

This is an example of how to use `apns4erl` with a pool of connections. We are using [worker_pool](https://github.com/inaka/worker_pool) for creating the pool.

## Run

After cloning this repo in your local you must replace the `priv/cert2.pem` and `priv/key2-noenc.pem` files by your own provided by Apple.

You need to replace the `apns-topic` in the module `apns_pool:push/2` also:

```erlang
push(DeviceId, Message) ->
  Notification = create_notification(Message),
  ApnsTopic = <<"com.inaka.myapp">>, % Replace by your own topic
  wpool:call(pool_name(), {push, DeviceId, ApnsTopic, Notification}).
```

Now the configuration is done, lets compile:

```
$ rebar3 compile
```

and run

```
  $ erl erl -pa _build/default/lib/*/ebin -s apns_pool
  Erlang/OTP 19 [erts-8.3] [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

  Eshell V8.3  (abort with ^G)
  1> 09:13:28.408 [info] Application lager started on node nonode@nohost
  09:13:28.408 [info] Application chatterbox started on node nonode@nohost
  09:13:28.408 [info] Application base64url started on node nonode@nohost
  09:13:28.411 [info] Application apns started on node nonode@nohost
  09:13:28.413 [info] Application worker_pool started on node nonode@nohost
  09:13:44.760 [info] Application apns_pool started on node nonode@nohost
```

you must wait until the last message is printed (`apns_pool started`), it could take some seconds until `apns_pool` loads completely.

## Pushing messages

You need a device id and calling `apns_pool:push/2` function:

```
1> DeviceId = <<"bd5c3ad01bbe4d884bf2fe8801ed77e94a71bc2e9de937c84f745f54eb4cb2f4">>.
<<"bd5c3ad01bbe4d884bf2fe8801ed77e94a71bc2e9de937c84f745f54eb4cb2f4">>
2> apns_pool:push(DeviceId, <<"hello from a pool">>).
{200,
 [{<<"apns-id">>,<<"1EF4ED9F-42FC-BA8B-450F-3BF6B09C72CB">>}],
 no_body}
```

Thats all!
