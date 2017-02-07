%%% @doc Contains helper functions for feedback.
%%%
%%% Copyright 2017 Erlang Solutions Ltd.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%% @end
%%% @copyright Inaka <hello@inaka.net>
%%%
-module(apns_feedback).
-author("Felipe Ripoll <felipe@inakanetworks.com>").

% API
-export([get_feedback/1]).

-export_type([feedback/0]).

-type feedback() :: {calendar:datetime(), string()}.
-type socket()   :: gen_tcp:socket().

%%%===================================================================
%%% API
%%%===================================================================

%% Requests for feedback to APNs. This requires Provider Certificate.
-spec get_feedback(timeout()) -> [feedback()] | {error, term()} | timeout.
get_feedback(Timeout) ->
  case open_feedback(Timeout) of
    {ok, Socket} ->
      wait_for_feedback(Socket, Timeout);
    {error, Reason} ->
      {error, Reason}
  end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

-spec open_feedback(timeout()) -> {ok, socket()} | {error, term()}.
open_feedback(Timeout) ->
  {ok, Host} = application:get_env(apns, feedback_host),
  {ok, Port} = application:get_env(apns, feedback_port),
  ssl:connect(Host, Port, ssl_opts(), Timeout).

-spec ssl_opts() -> list().
ssl_opts() ->
  Opts = case application:get_env(apns, keyfile) of
    {ok, KeyFile} -> [{keyfile, KeyFile}];
    undefined     -> []
  end,
  {ok, Certfile} = application:get_env(apns, certfile),
  [{certfile, Certfile} | Opts].

-spec wait_for_feedback(socket(), timeout()) -> [feedback()] | timeout.
wait_for_feedback(Socket, Timeout) ->
  wait_for_feedback(Socket, <<>>, [], Timeout).

-spec wait_for_feedback(socket(), binary(), [feedback()], timeout()) ->
  [feedback()] | timeout.
wait_for_feedback(Socket, Buffer, Feedback, Timeout) ->
  receive
    {ssl, Socket, Data} ->
      build_feedback(Socket, Data, Buffer, Feedback, Timeout)
  after Timeout ->
    timeout
  end.

-spec build_feedback(socket(), binary(), binary(), [feedback()], timeout()) ->
  [feedback()] | timeout.
build_feedback(Socket, Data, Buffer, Feedback, Timeout) ->
  case <<Buffer/binary, Data/binary>> of
    << TimeT:4/big-unsigned-integer-unit:8
     , Length:2/big-unsigned-integer-unit:8
     , Token:Length/binary
     , Rest/binary>> ->
     NewFeedback = [{ apns_utils:seconds_to_timestamp(TimeT)
                    , apns_utils:bin_to_hexstr(Token)
                    } | Feedback],
     case size(Rest) of
       0 -> NewFeedback;
       _ -> wait_for_feedback(Socket, Rest, NewFeedback, Timeout)
     end;
    NewBuffer ->
      wait_for_feedback(Socket, NewBuffer, Feedback, Timeout)
  end.
