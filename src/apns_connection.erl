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

-export([start_link/1, start_link/2, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([send_message/2, stop/1]).

-record(state, {socket :: ssl:sslsocket()}).
-type state() :: #state{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc  Sends a message to apple through the connection
%% @spec send_message(conn_id(), #apns_msg{}) -> ok
-spec send_message(conn_id(), #apns_msg{}) -> ok.
send_message(ConnId, Msg) ->
  gen_server:cast(ConnId, Msg).

%% @doc  Stops the connection
%% @spec stop(conn_id()) -> ok
-spec stop(conn_id()) -> ok.
stop(ConnId) ->
  gen_server:cast(ConnId, stop).

%% @hidden
-spec start_link(atom(), #apns_connection{}) -> {ok, pid()} | {error, {already_started, pid()}}.
start_link(Name, Connection) ->
  gen_server:start_link({local, Name}, ?MODULE, Connection, []).
-spec start_link(#apns_connection{}) -> {ok, pid()}.
start_link(Connection) ->
  gen_server:start_link(?MODULE, Connection, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Server implementation, a.k.a.: callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @hidden
-spec init(#apns_connection{}) -> {ok, state()} | {stop, term()}.
init(Connection) ->
  ok = ssl:seed(Connection#apns_connection.ssl_seed),
  case ssl:connect(
         Connection#apns_connection.apple_host,
         Connection#apns_connection.apple_port,
         [{certfile, Connection#apns_connection.cert_file}, {mode, binary}],
         Connection#apns_connection.timeout) of
    {ok, Socket} ->
      {ok, #state{socket = Socket}};
    {error, Reason} ->
      {stop, Reason}
  end.

%% @hidden
-spec handle_call(X, reference(), state()) -> {stop, {unknown_request, X}, {unknown_request, X}, state()}.
handle_call(Request, _From, State) ->
  {stop, {unknown_request, Request}, {unknown_request, Request}, State}.

%% @hidden
-spec handle_cast(stop | #apns_msg{}, state()) -> {noreply, state()} | {stop, normal | {error, term()}, state()}.
handle_cast(Msg, State) when is_record(Msg, apns_msg) ->
  Socket = State#state.socket,
  Payload = build_payload([{alert, Msg#apns_msg.alert},
                           {badge, Msg#apns_msg.badge},
                           {sound, Msg#apns_msg.sound}]),
  BinToken = hexstr_to_bin(Msg#apns_msg.device_token),
  case send_payload(Socket, BinToken, Payload) of
    ok ->
      {noreply, State};
    {error, Reason} ->
      {stop, {error, Reason}, State}
  end;
handle_cast(stop, State) ->
  {stop, normal, State}.

%% @hidden
-spec handle_info({ssl_closed, ssl:sslsocket()} | X, state()) -> {stop, ssl_closed | {unknown_request, X}, state()}.
handle_info({ssl_closed, SslSocket}, State = #state{socket = SslSocket}) ->
  {stop, ssl_closed, State};
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
build_payload(Params) ->
  build_payload(Params, []).
build_payload([{Key,Value}|Params], Payload) -> 
  case Value of
    Value when is_list(Value) ->
      build_payload(Params, Payload ++ [lists:flatten(["\"",atom_to_list(Key),"\":\"",Value,"\""])]);
    Value when is_integer(Value) ->
      build_payload(Params, Payload ++ [lists:flatten(["\"",atom_to_list(Key),"\":",integer_to_list(Value)])]);
    _ ->
      build_payload(Params,Payload)
  end;
build_payload([], Payload) ->
  ["{\"aps\":{", string:join(Payload,",") ,"}}"].

send_payload(Socket, BinToken, Payload) -> 
    PayloadLength = length(Payload),
    BinPayload = list_to_binary(Payload),
    Packet = <<0:8, 32:16/big,
               %%16#ac812b2d723f40f206204402f1c870c8d8587799370bd41d6723145c4e4ebbd7:256/big,
               BinToken/binary,
               PayloadLength:16/big,
               BinPayload/binary>>,
    ssl:send(Socket, Packet).

hexstr_to_bin(S) ->
  hexstr_to_bin(S, []).
hexstr_to_bin([], Acc) ->
  list_to_binary(lists:reverse(Acc));
hexstr_to_bin([X,Y|T], Acc) ->
  {ok, [V], []} = io_lib:fread("~16u", [X,Y]),
  hexstr_to_bin(T, [V | Acc]).
