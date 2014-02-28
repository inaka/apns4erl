%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%%% @copyright (C) 2010 Fernando Benavides <fernando.benavides@inakanetworks.com>
%%% @doc apns4erl connection process
%%% @end
%%%-------------------------------------------------------------------
-module(apns_connection).
-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').

-behaviour(gen_server).

-include("apns.hrl").
-include("localized.hrl").

-export([start_link/1, start_link/2, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([send_message/2, stop/1]).
-export([build_payload/1]).

-record(state, {out_socket        :: tuple(),
                in_socket         :: tuple(),
                connection        :: #apns_connection{},
                in_buffer = <<>>  :: binary(),
                out_buffer = <<>> :: binary(),
                msg_queue         :: queue(),
                empty = true      :: boolean()}).
-type state() :: #state{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc  Sends a message to apple through the connection
-spec send_message(apns:conn_id(), #apns_msg{}) -> ok.
send_message(ConnId, Msg) ->
  gen_server:cast(ConnId, Msg).

%% @doc  Stops the connection
-spec stop(apns:conn_id()) -> ok.
stop(ConnId) ->
  gen_server:cast(ConnId, stop).

%% @hidden
-spec start_link(atom(), #apns_connection{}) -> {ok, pid()} | {error, {already_started, pid()}}.
start_link(Name, Connection) ->
  gen_server:start_link({local, Name}, ?MODULE, Connection, []).
%% @hidden
-spec start_link(#apns_connection{}) -> {ok, pid()}.
start_link(Connection) ->
  gen_server:start_link(?MODULE, Connection, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Server implementation, a.k.a.: callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @hidden
-spec init(#apns_connection{}) -> {ok, state()} | {stop, term()}.
init(Connection) ->
  case Connection#apns_connection.delay_connection of
    true ->
      {_Result, State} = handle_info(reconnect_feedback, #state{connection = Connection, msg_queue = queue:new(), empty = true}),
      {ok, State};
    false -> open_connections(Connection)
  end.
%% @hidden
open_connections(Connection) ->
  try
    case open_out(Connection) of
      {ok, OutSocket} -> case open_feedback(Connection) of
          {ok, InSocket} -> {ok, #state{out_socket=OutSocket, in_socket=InSocket, connection=Connection, msg_queue=queue:new(), empty=true}};
          {error, Reason} -> {stop, Reason}
        end;
      {error, Reason} -> {stop, Reason}
    end
  catch
    _:{error, Reason2} -> {stop, Reason2}
  end.

%% @hidden
open_out(Connection) ->
  KeyFile = case Connection#apns_connection.key_file of
    undefined -> [];
    Filename -> [{keyfile, filename:absname(Filename)}]
  end,
  SslOpts = [
    {certfile, filename:absname(Connection#apns_connection.cert_file)},
    {mode, binary} | KeyFile
  ],
  RealSslOpts = case Connection#apns_connection.cert_password of
    undefined -> SslOpts;
    Password -> [{password, Password} | SslOpts]
  end,
  case ssl:connect(
    Connection#apns_connection.apple_host,
    Connection#apns_connection.apple_port,
    RealSslOpts,
    Connection#apns_connection.timeout
  ) of
    {ok, OutSocket} -> {ok, OutSocket};
    {error, Reason} -> {error, Reason}
  end.

%% @hidden
open_feedback(Connection) ->
  KeyFile = case Connection#apns_connection.key_file of
    undefined -> [];
    Filename -> [{keyfile, filename:absname(Filename)}]
  end,
  SslOpts = [
    {certfile, filename:absname(Connection#apns_connection.cert_file)},
    {mode, binary} | KeyFile
  ],
  RealSslOpts = case Connection#apns_connection.cert_password of
    undefined -> SslOpts;
    Password -> [{password, Password} | SslOpts]
  end,
  case ssl:connect(
    Connection#apns_connection.feedback_host,
    Connection#apns_connection.feedback_port,
    RealSslOpts,
    Connection#apns_connection.timeout
  ) of
    {ok, InSocket} -> {ok, InSocket};
    {error, Reason} -> {error, Reason}
  end.

%% @hidden
-spec handle_call(X, reference(), state()) -> {stop, {unknown_request, X}, {unknown_request, X}, state()}.
handle_call(Request, _From, State) ->
  {stop, {unknown_request, Request}, {unknown_request, Request}, State}.

%% @hidden
-spec handle_cast(stop | #apns_msg{}, state()) -> {noreply, state()} | {stop, normal | {error, term()}, state()}.
handle_cast(Msg, State = #state{empty = false}) ->
  {noreply, queue_msg(Msg, State)};

handle_cast(Msg, State = #state{out_socket = undefined}) ->
  case handle_info(reconnect_out, State) of
    {noreply, State1} -> case State1#state.out_socket of
          undefined -> {noreply, queue_msg(Msg, State1)};
          _OutSocket -> handle_cast(Msg, State1)
        end;
    Error -> Error
  end;

handle_cast(Msg, State) when is_record(Msg, apns_msg) ->
  case send_msg(Msg, State) of
    ok -> {noreply, State};
    {error, Reason} -> 
      Connection = State#state.connection,
      case Connection#apns_connection.retry_connection of
        false -> {stop, {error, Reason}, State};
        true -> 
          State1 = queue_msg(Msg, State),
          {noreply, retry_later(out, State1)} 
      end
  end;

handle_cast(stop, State) ->
  {stop, normal, State}.

%% @hidden
-spec handle_info({ssl, tuple(), binary()} | {ssl_closed, tuple()} | X, state()) -> {noreply, state()} | {stop, ssl_closed | {unknown_request, X}, state()}.
handle_info({ssl, SslSocket, Data}, State = #state{out_socket = SslSocket,
                                                   connection =
                                                     #apns_connection{error_fun = Error},
                                                   out_buffer = CurrentBuffer}) ->
  case <<CurrentBuffer/binary, Data/binary>> of
    <<Command:1/unit:8, StatusCode:1/unit:8, MsgId:4/binary, Rest/binary>> ->
      case Command of
        8 -> %% Error
          Status = parse_status(StatusCode),
          try Error(MsgId, Status) of
            stop -> throw({stop, {msg_error, MsgId, Status}, State});
            _ -> noop
          catch
            _:ErrorResult ->
              error_logger:error_msg("Error trying to inform error (~p) msg ~p:~n\t~p~n",
                                     [Status, MsgId, ErrorResult])
          end,
          case erlang:size(Rest) of
            0 -> {noreply, State#state{out_buffer = <<>>}}; %% It was a whole package
            _ -> handle_info({ssl, SslSocket, Rest}, State#state{out_buffer = <<>>})
          end;
        Command ->
          throw({stop, {unknown_command, Command}, State})
      end;
    NextBuffer -> %% We need to wait for the rest of the message
      {noreply, State#state{out_buffer = NextBuffer}}
  end;
handle_info({ssl, SslSocket, Data}, State = #state{in_socket  = SslSocket,
                                                   connection =
                                                     #apns_connection{feedback_fun = Feedback},
                                                   in_buffer  = CurrentBuffer
                                                  }) ->
  case <<CurrentBuffer/binary, Data/binary>> of
    <<TimeT:4/big-unsigned-integer-unit:8,
      Length:2/big-unsigned-integer-unit:8,
      Token:Length/binary,
      Rest/binary>> ->
      try Feedback({apns:timestamp(TimeT), bin_to_hexstr(Token)})
      catch
        _:Error ->
          error_logger:error_msg("Error trying to inform feedback token ~p:~n\t~p~n", [Token, Error])
      end,
      case erlang:size(Rest) of
        0 -> {noreply, State#state{in_buffer = <<>>}}; %% It was a whole package
        _ -> handle_info({ssl, SslSocket, Rest}, State#state{in_buffer = <<>>})
      end;
    NextBuffer -> %% We need to wait for the rest of the message
      {noreply, State#state{in_buffer = NextBuffer}}
  end;

handle_info({ssl_closed, SslSocket}, State = #state{in_socket = SslSocket,
                                                    connection= Connection}) ->
  error_logger:info_msg("Feedback server disconnected. Waiting ~p millis to connect again...~n",
                        [Connection#apns_connection.feedback_timeout]),
  {noreply, retry_later(feedback, State)};

handle_info(reconnect_feedback, State = #state{connection = Connection}) ->
  error_logger:info_msg("Reconnecting the Feedback server...~n"),
  try
    case open_feedback(Connection) of
      {ok, InSocket} -> {noreply, State#state{in_socket = InSocket}};
      {error, Reason} -> case Connection#apns_connection.retry_connection of 
          false -> {stop, {in_closed, Reason}, State};
          true -> {noreply, retry_later(feedback, State)}
        end
    end
  catch
    _:{error, Reason2} -> {stop, {out_closed, Reason2}, State}
  end;

handle_info(reconnect_out, State = #state{out_socket = undefined, connection = Connection}) ->
  error_logger:info_msg("Reconnecting to APNS server...~n"),
  try
    case open_out(Connection) of
      {ok, OutSocket} -> 
        error_logger:info_msg("Connected to APNS server.~n"),
        State1 = drain_queue(State#state{out_socket = OutSocket}),
        case State1#state.empty of
          false -> {noreply, retry_later(out, State1)}; %% A non empty queue means retry_connection is set
          true -> {noreply, State1}
        end;
      {error, Reason} -> case Connection#apns_connection.retry_connection of 
          false -> {stop, {out_closed, Reason}, State};
          true -> {noreply, retry_later(out, State)}
        end
    end
  catch
    _:{error, Reason2} -> {stop, {out_closed, Reason2}, State}
  end;

handle_info({ssl_closed, SslSocket}, State = #state{out_socket = SslSocket}) ->
  error_logger:info_msg("APNS disconnected~n"),
  {noreply, State#state{out_socket=undefined}};

handle_info(Request, State) ->
  {stop, {unknown_request, Request}, State}.

%% @hidden
-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) -> ok.

%% @hidden
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->  {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec send_msg(#apns_msg{}, state()) -> ok | {error, term()}.
send_msg(Msg, #state{out_socket=Socket}) ->
  Payload = build_payload(Msg),
  BinToken = hexstr_to_bin(Msg#apns_msg.device_token),
  send_payload(Socket, Msg#apns_msg.id, Msg#apns_msg.expiry, BinToken, Payload).

-spec queue_msg(#apns_msg{}, state()) -> state().
queue_msg(Msg, State) ->
  State#state{msg_queue=queue:in(Msg, State#state.msg_queue), empty=false}.

-spec drain_queue(state()) -> state().
drain_queue(State = #state{empty = true}) ->
  State;

drain_queue(State=#state{msg_queue = Queue}) ->
  case queue:peek(State#state.msg_queue) of
    empty -> State#state{empty = true};
    {value, Msg} -> 
      case send_msg(Msg, State#state.out_socket) of
        ok -> %% Keep going
          Queue = queue:drop(State#state.msg_queue),
          drain_queue(State#state{msg_queue = Queue});
        {error, Reason} -> 
          error_logger:info_msg("Error:~p sending message:~p to APNS. ~n", [Reason, Msg#apns_msg.id]),
          State
      end
  end.

-spec retry_later(out|feedback, state()) -> state().
retry_later(out, State = #state{connection = Connection}) ->
  _Timer = erlang:send_after(Connection#apns_connection.retry_interval, self(), reconnect_out),
  State#state{out_socket=undefined};

retry_later(feedback, State = #state{connection = Connection}) ->
  _Timer = erlang:send_after(Connection#apns_connection.feedback_timeout, self(), reconnect_feedback),
  State#state{in_socket = undefined}.

build_payload(#apns_msg{alert = Alert,
                        badge = Badge,
                        sound = Sound,
                        apns_extra=Apns_Extra,
                        content_available = Content_Available,
                        extra = Extra}) ->
    build_payload([{alert, Alert},
                   {badge, Badge},
                   {sound, Sound}] ++ Apns_Extra, Extra, Content_Available).

build_payload(Params, Extra, Content_Available) ->
  apns_mochijson2:encode({[{<<"aps">>,
                            do_build_payload(Params, Content_Available)} | Extra]}).

do_build_payload(Params, Content_Available) when Content_Available ->
  do_build_payload(Params, [{<<"content-available">>, 1}]);

do_build_payload(Params, Content_Available) when Content_Available == false ->
  do_build_payload(Params, []);

do_build_payload([{Key,Value}|Params], Payload) ->
  case Value of
    Value when is_list(Value); is_binary(Value) ->
      do_build_payload(Params, [{atom_to_binary(Key, utf8), unicode:characters_to_binary(Value)} | Payload]);
    Value when is_integer(Value) ->
      do_build_payload(Params, [{atom_to_binary(Key, utf8), Value} | Payload]);
    #loc_alert{action = Action,
               args   = Args,
               body   = Body,
               image  = Image,
               key    = LocKey} ->
      Json = {case Body of
                none -> [];
                Body -> [{<<"body">>, unicode:characters_to_binary(Body)}]
              end ++ case Action of
                       none -> [];
                       Action -> [{<<"action-loc-key">>, unicode:characters_to_binary(Action)}]
                     end ++ case Image of
                              none -> [];
                              Image -> [{<<"launch-image">>, unicode:characters_to_binary(Image)}]
                            end ++
                [{<<"loc-key">>, unicode:characters_to_binary(LocKey)},
                 {<<"loc-args">>, lists:map(fun unicode:characters_to_binary/1, Args)}]},
      do_build_payload(Params, [{atom_to_binary(Key, utf8), Json} | Payload]);
    _ ->
      do_build_payload(Params,Payload)
  end;
do_build_payload([], Payload) ->
  {Payload}.

-spec send_payload(tuple(), binary(), non_neg_integer(), binary(), iolist()) -> ok | {error, term()}.
send_payload(Socket, MsgId, Expiry, BinToken, Payload) ->
    BinPayload = list_to_binary(Payload),
    PayloadLength = erlang:size(BinPayload),
    Packet = [<<1:8, MsgId/binary, Expiry:4/big-unsigned-integer-unit:8,
                32:16/big,
                BinToken/binary,
                PayloadLength:16/big,
                BinPayload/binary>>],
    error_logger:info_msg("Sending msg ~p (expires on ~p)~n",
                         [MsgId, Expiry]),
    ssl:send(Socket, Packet).

hexstr_to_bin(S) ->
  hexstr_to_bin(S, []).
hexstr_to_bin([], Acc) ->
  list_to_binary(lists:reverse(Acc));
hexstr_to_bin([$ |T], Acc) ->
    hexstr_to_bin(T, Acc);
hexstr_to_bin([X,Y|T], Acc) ->
  {ok, [V], []} = io_lib:fread("~16u", [X,Y]),
  hexstr_to_bin(T, [V | Acc]).

bin_to_hexstr(Binary) ->
    L = size(Binary),
    Bits = L * 8,
    <<X:Bits/big-unsigned-integer>> = Binary,
    F = lists:flatten(io_lib:format("~~~B.16.0B", [L * 2])),
    lists:flatten(io_lib:format(F, [X])).

parse_status(0) -> no_errors;
parse_status(1) -> processing_error;
parse_status(2) -> missing_token;
parse_status(3) -> missing_topic;
parse_status(4) -> missing_payload;
parse_status(5) -> missing_token_size;
parse_status(6) -> missing_topic_size;
parse_status(7) -> missing_payload_size;
parse_status(8) -> invalid_token;
parse_status(_) -> unknown.
