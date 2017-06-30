-module(apns_pool).
-author("Felipe Ripoll <felipe@inakanetworks.com>").

-behaviour(application).

%% Application callbacks
-export([ start/0
        , push/2
        ]).

%% Application callbacks
-export([ start/2
        , stop/1
        ]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
  apns_pool_sup:start_link(pool_name()).

stop(_State) ->
  ok.

%%%===================================================================
%%% API
%%%===================================================================

start() ->
  application:ensure_all_started(apns_pool).

push(DeviceId, Message) ->
  Notification = create_notification(Message),
  ApnsTopic = <<"com.inaka.myapp">>, % Replace by your own topic
  wpool:call(pool_name(), {push, DeviceId, ApnsTopic, Notification}).

%%%===================================================================
%%% private functions
%%%===================================================================

create_notification(Message) ->
  #{<<"aps">> =>
     #{ <<"alert">> => Message
      , <<"sound">> => <<"default">>
      , <<"badge">> => 1}
   }.

pool_name() -> ?MODULE.
