-module(apns_pool_sup).
-author("Felipe Ripoll <felipe@inakanetworks.com>").
-behaviour(supervisor).

%% API
-export([ start_link/1
        ]).

%% Supervisor callbacks
-export([ init/1
        ]).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(PoolName) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, PoolName).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init(PoolName) ->
  WPoolOptions  = [ {overrun_warning, infinity}
                  , {overrun_handler, {error_logger, warning_report}}
                  , {workers, 50}
                  , {worker, {apns_pool_worker, apns4erl_config()}}
                  ],

  SupFlags = #{ strategy  => one_for_one
              , intensity => 1000
              , period    => 3600
              },

  Children = [#{ id       => wpool
               , start    => {wpool, start_pool, [PoolName, WPoolOptions]}
               , restart  => permanent
               , shutdown => 5000
               , type     => supervisor
               , modules  => [wpool]
               }],

  {ok, {SupFlags, Children}}.

%%%===================================================================
%%% Private functions
%%%===================================================================

apns4erl_config() ->
  % this should be in a config file but its fine for an example :)
  #{ name       => undefined
   , apple_host => "api.push.apple.com"
   , apple_port => 443
   , certfile   => "priv/cert2.pem"
   , keyfile    => "priv/key2-noenc.pem"
   , timeout    => 10000
   , type       => cert
   }.
