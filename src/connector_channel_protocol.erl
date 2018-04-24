-module(connector_channel_protocol).

-behaviour(gen_server).

%% TEST
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("print.hrl").
-include("config.hrl").
-include("db.hrl").

-export([start_link/4]).
-export([recv_order_frame_from_client/2]).
-export([shutdown/1]).

-export([
         init/1,
         init/4,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-define(PROTOCOL_3761, 3761).
-define(PROTOCOL_645, 645).

%% Timer msg
-define(MY_TIMEOUT_MSG, my_timeout_msg).

%% Maxinum number of timeout
-define(MAXINUM_NUMBER, 3).

-define(ONLINE_STATUS, "1").
-define(OFFLINE_STATUS, "0").

%% myTimeout record
-record(myTimeout, {
          numberOfTimeout = 0 :: non_neg_integer(),
          timerRef = undefined :: undefined | reference()
}).
-type myTimeout() :: #myTimeout{}.

-record(state, {
          listenerPid :: ranch:ref(),
          selfPid :: pid(),
          socket :: inet:socket(),
          transport :: module(),
          lastPacket = <<>> :: binary(),
          gatewayType :: string() | undefined,
          gatewayId :: string() | undefined,
          gatewayProtocol,
          activeReportPid = undefined :: pid() | undefined,
          clientSendRecvPid = undefined :: pid() | undefined,
          myTimeout = undefined :: myTimeout() | undefined
}).

start_link(ListenerPid, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [ListenerPid, Socket, Transport, Opts]).

%% Recv order frame(hex) from client process 
recv_order_frame_from_client(Pid, OrderFrameHex) ->
    gen_server:cast(Pid, {clientOrderFrame, OrderFrameHex}).

shutdown(Pid) ->
    gen_server:cast(Pid, shutdownGateway).

init([]) ->
    {ok, undefined}.

init(ListenerPid, Socket, Transport, _Opts) ->
    SelfPid = self(),
    ok = proc_lib:init_ack({ok, SelfPid}),
    ok = ranch:accept_ack(ListenerPid),
    ok = Transport:setopts(Socket, [{active, once}]),

    ?PRINT("Gateway connect pid:~p~n", [SelfPid]),

    %% Trap_exit is true
    erlang:process_flag(trap_exit, true),

    State = #state{
               listenerPid = ListenerPid,
               selfPid = SelfPid,
               socket = Socket,
               transport = Transport,
               lastPacket = <<>>,
               gatewayType = undefined,
               gatewayId = undefined,
               gatewayProtocol = undefined,
               activeReportPid = undefined,
               clientSendRecvPid = undefined,
               myTimeout = undefined
              },
    {ok, [{_, TIMEOUT}, _, _, _]} = file:consult(?TIME_OPTS_FILEPATH),
    gen_server:enter_loop(?MODULE, [], State, TIMEOUT).

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({clientOrderFrame, OrderFrameHex}, State) ->
    #state{
       gatewayId = GatewayId,
       socket = Socket,
       transport = Transport,
       activeReportPid = _ActiveReportPid,
       clientSendRecvPid = _ClientSendRecvPid,
       myTimeout = _MyTimeout   
      } = State,

    %% Log record: gateway order send log
    connector_event_server:gateway_order_send_log(GatewayId, OrderFrameHex),

    OrderFrameBinary = hex_util:to_bin(OrderFrameHex),
    ok = Transport:send(Socket, OrderFrameBinary),
    ?PRINT("OrderFrame is send: ~p~n", [OrderFrameHex]),
    {noreply, State};
handle_cast(shutdownGateway, State) ->
    ?PRINT("shutdownGateway~n", []),
    {stop, shutdown, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    #state{
       gatewayId = GatewayId,
       selfPid = SelfPid,
       socket = Socket,
       transport = Transport,
       activeReportPid = ActiveReportPid,
       clientSendRecvPid = ClientSendRecvPid,
       myTimeout = MyTimeout
      } = State,

    ?PRINT("~p terminate~n", [SelfPid]),
    ?PRINT("Reason:~p, State:~p~n", [Reason, State]),

    %% Close socket
    Transport:close(Socket),
    %% Cancel timer
    cancel_timer_of_mytimeout(MyTimeout),
    %% stop activereportpid
    stop_active_report_process(ActiveReportPid, Reason),
    %% stop clientsendrecvpid
    stop_client_send_recv_process(ClientSendRecvPid, Reason),
    case {GatewayId, Reason} of
        {undefined, _} ->
            ok;
        {_, shutdown} -> %% 覆盖网关信息，不需要删除对应的（gatewayId, pid）
            ok;
        _ ->
            connector_gateway_to_store_server:delete_gateway_pid(GatewayId)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info({tcp_closed, _Socket}, State) ->
    ?PRINT("tcp_closed!!!~n", []),
    Status = "00 Abnormal Disconnect",
    connector_event_server:gateway_status_log(State#state.gatewayId, Status),
    update_gateway_status(State#state.gatewayId, ?OFFLINE_STATUS),
    {stop, normal, State};
handle_info({tcp_error, _Socket, Reason}, State) ->
    ?PRINT("tcp_error~n", []),
    Status = "00 Abnormal Disconnect",
    connector_event_server:gateway_status_log(State#state.gatewayId, Status),
    update_gateway_status(State#state.gatewayId, ?OFFLINE_STATUS),
    {stop, Reason, State};
%% In ?TIMEOUT time don't receive login packet
handle_info(timeout, State) ->
    ?PRINT("timeout~n", []),
    update_gateway_status(State#state.gatewayId, ?OFFLINE_STATUS),
    {stop, normal, State};
%% In ?TIME time don't receive any packet
handle_info({timeout, TimerRefTmp, ?MY_TIMEOUT_MSG}, State) ->
    io:format("start timeout~n"),
    {ok, [_, {_ ,TIMER_TIME}, _, _]} = file:consult(?TIME_OPTS_FILEPATH),
    io:format("TIMER_TIME:~p~n",[TIMER_TIME]),  
    MyTimeout = State#state.myTimeout,
    SelfPid = State#state.selfPid,
    #myTimeout{
       numberOfTimeout = NumberOfTimeout,
       timerRef = TimerRef
      } = MyTimeout,
    mytimer:cancel_timer(TimerRefTmp),
    case {NumberOfTimeout < ?MAXINUM_NUMBER, TimerRef =:= TimerRefTmp} of
        {true, true} ->
            NewMyTimeout = #myTimeout{
               numberOfTimeout = NumberOfTimeout + 1,
               timerRef = mytimer:start_timer(TIMER_TIME, SelfPid, ?MY_TIMEOUT_MSG)
              },
            {noreply, State#state{myTimeout = NewMyTimeout}};
        {true, false} ->
            {noreply, State};
        {false, _} ->
            mytimer:cancel_timer(TimerRef),
            io:format("connection closed!!!"),
            Status = "00 Normal Disconnect",
            connector_event_server:gateway_status_log(State#state.gatewayId, Status),
            update_gateway_status(State#state.gatewayId, ?OFFLINE_STATUS),
            {stop, normal, State}
    end;

handle_info({'EXIT', From, normal}, State) ->
    ?PRINT("From:~p stop~n", [From]),
    update_gateway_status(State#state.gatewayId, ?OFFLINE_STATUS),
    {stop, normal, State};
handle_info({'EXIT', From, Reason}, State) ->
    #state{
       selfPid = SelfPid,
       gatewayType = GatewayType,
       gatewayId = GatewayId,
       activeReportPid = ActiveReportPid,
       clientSendRecvPid = ClientSendRecvPid
      } = State,
    case From of
        ActiveReportPid ->
            ReportFlag = true,
            {ok, NewActiveReportPid} = start_active_report_process(GatewayType, GatewayId, SelfPid, ReportFlag),
            {noreply, State#state{activeReportPid = NewActiveReportPid}};
        ClientSendRecvPid ->
            {ok, NewClientSendRecvPid} = start_client_send_recv_process(GatewayType, GatewayId, SelfPid),
            {noreply, State#state{clientSendRecvPid = NewClientSendRecvPid}};
        _ ->
            {stop, Reason, State}
    end;

%%------------------------------------------------------------------------------------------
%% 376.1 protocol
%%------------------------------------------------------------------------------------------

%% Concentrator
handle_info({tcp, Socket, <<16#68, Len:16/little-integer, Len:16/little-integer, 16#68, Rest/binary>> = Packet}, 
            State) when (byte_size(Rest) =:= ((Len bsr 2) + 2)) ->
    #state{
       selfPid = SelfPid,
       gatewayProtocol = GatewayProtocol,
       socket = Socket,
       transport = Transport,
       lastPacket = _LastPacket,
       activeReportPid = ActiveReportPid,
       clientSendRecvPid = ClientSendRecvPid,
       myTimeout = MyTimeout
      } = State,

    ok = Transport:setopts(Socket, [{active, once}]),
    
    StateTmp = handle_3761_packet(Packet, State),
    
    cancel_timer_of_mytimeout(MyTimeout),
    {ok, [_, {_ ,TIMER_TIME}, _, _]} = file:consult(?TIME_OPTS_FILEPATH),
    NewMyTimeout = #myTimeout{
                   numberOfTimeout = 0,
                   timerRef = mytimer:start_timer(TIMER_TIME, SelfPid, ?MY_TIMEOUT_MSG)
                  },
    NewState = StateTmp#state{
                 lastPacket = <<>>, %% 完整的一帧，前面遗留的报文都丢弃
                 myTimeout = NewMyTimeout
                },
    {noreply, NewState};

%%-----------------------------------------------------------------------------------------
%% ZigBee 645 protocol
%%-----------------------------------------------------------------------------------------

%% ZigBee gateway login packet of hex: "FA071302FA02" ++ "000663007098"(GatewayId) ++ "FAFF"
handle_info({tcp, Socket, Packet = <<16#FA, 16#07, 16#13, 16#02, 16#FA, 16#02,
                            GatewayIdBinary:?DEVICE_ID_BYTES_SIZE/binary-unit:8, 16#FA, 16#FF>>},
            State) ->
    #state{
       selfPid = SelfPid,
       transport = Transport,
       socket = Socket
      } = State,
    io:format("State:~p~n",[State]),
    ok = Transport:setopts(Socket, [{active, once}]),

    GatewayId = hex_util:to_hex(GatewayIdBinary),
    FrameHex = hex_util:to_hex(Packet),

    ?PRINT("Login :~p~n", [FrameHex]),
    try gen_concentrator_addr_binary(GatewayId) of
        AddrBinary ->
            Seq = 0,
            ResponFrame = gen_respon_frame(AddrBinary, Seq),
            ?PRINT("ZigBee respon frame:~p~n", [hex_util:to_hex(ResponFrame)]),
            ok = Transport:send(Socket, ResponFrame)
    catch
        _:_ ->
            ok
    end,

    {ok, {Ip, Port}} = Transport:peername(Socket),
    %% Log record: gateway login log
    Status = "01",
    io:format("ip:~p port:~p~n",[Ip, Port]),
    connector_event_server:gateway_status_log(GatewayId, Status),
    gateway_status_store(GatewayId, ?ONLINE_STATUS), 
    case inet:ntoa(Ip) of
        {error, _} ->
            connector_event_server:gateway_login_log(GatewayId, FrameHex);
        Address ->
            Msg = string:join([Address, FrameHex], " "),
            ?PRINT("Address:~p~n", [Msg]),
            connector_event_server:gateway_login_log(GatewayId, Msg)
    end,

    
    case connector_db_store_server:get_gateway_type(GatewayId) of
        {error, _} ->
            ?ERROR("GatewayId:~p isn't found~n", [GatewayId]),
            {stop, normal, State};
        {ok, GatewayType} ->
            ?PRINT("~p:~p Gateway:~p/~p is login~n", [?FILE, ?LINE, GatewayType, GatewayId]),
            %% Create active report process and client send/recv process
            %% TODO
            case connector_gateway_to_store_server:lookup_gateway_pid(GatewayId) of
                {ok, OldPid} when (OldPid =/= SelfPid) ->
                    ?MODULE:shutdown(OldPid);
                _ ->
                    ok
            end,
            connector_gateway_to_store_server:insert_gateway_pid(GatewayId, SelfPid),

            ReportFlag = true,
            {ok, ActiveReportPid} = start_active_report_process(GatewayType, GatewayId, SelfPid, ReportFlag),
            {ok, ClientSendRecvPid} = start_client_send_recv_process(GatewayType, GatewayId, SelfPid),
            io:format("selfpid:~p clientpid:~p~n",[SelfPid, ClientSendRecvPid]),

            %% End.

            %% Gateway online need to active report
            connector_active_report:active_report_645_packet(ActiveReportPid, Packet),
            {ok, [_, {_ ,TIMER_TIME}, _, _]} = file:consult(?TIME_OPTS_FILEPATH),
            MyTimeout = #myTimeout{
                           numberOfTimeout = 0,
                           timerRef = mytimer:start_timer(TIMER_TIME, SelfPid, ?MY_TIMEOUT_MSG)
                          },
            NewState = State#state{
                         lastPacket = <<>>, %% 完整的一帧，前面遗留的报文都丢弃
                         gatewayType = GatewayType,
                         gatewayId = GatewayId,
                         gatewayProtocol = ?PROTOCOL_645,
                         activeReportPid = ActiveReportPid,
                         clientSendRecvPid = ClientSendRecvPid,
                         myTimeout = MyTimeout
                        },
            {noreply, NewState}
    end;
%% Zigbee heart beat packet Hex: "00"
handle_info({tcp, Socket, <<0>>}, State) ->
    #state{
       selfPid = SelfPid,
       socket = Socket,
       gatewayId = GatewayId,
       transport = Transport,
       myTimeout = MyTimeout
      } = State,

    ok = Transport:setopts(Socket, [{active, once}]),

    ?PRINT("Heart beat:~p~n", ["00"]),

    %% Log record: gateway heart log
    connector_event_server:gateway_heart_beat_log(GatewayId, "00"),
    update_gateway_status(GatewayId, ?ONLINE_STATUS), 

    try gen_concentrator_addr_binary(GatewayId) of
        AddrBinary ->
            Seq = 0,
            ResponFrame = gen_respon_frame(AddrBinary, Seq),
            ?PRINT("ZigBee respon frame:~p~n", [hex_util:to_hex(ResponFrame)]),
            ok = Transport:send(Socket, ResponFrame)
    catch
        _:_ ->
            ok
    end,

    %% Cancel timer
    cancel_timer_of_mytimeout(MyTimeout),
    {ok, [_, {_ ,TIMER_TIME}, _, _]} = file:consult(?TIME_OPTS_FILEPATH),
    NewMyTimeout = #myTimeout{
                      numberOfTimeout = 0,
                      timerRef = mytimer:start_timer(TIMER_TIME, SelfPid, ?MY_TIMEOUT_MSG)
                     },

    NewState = State#state{
                 lastPacket = <<>>, %% 完整的一帧，前面遗留的报文都丢弃
                 myTimeout = NewMyTimeout
                },
    {noreply, NewState};

handle_info({tcp, Socket, Packet}, State = #state{gatewayProtocol = undefined}) ->
    #state{
       selfPid = SelfPid,
       socket = Socket,
       gatewayId = GatewayId,
       transport = Transport,
       myTimeout = MyTimeout
      } = State,

    ok = Transport:setopts(Socket, [{active, once}]),

    ?PRINT("Before login packet recv abnormal packet: ~p~n", [hex_util:to_hex(Packet)]),
    {ok, [{_, TIMEOUT}, _, _, _]} = file:consult(?TIME_OPTS_FILEPATH),
    {noreply, State, TIMEOUT};

handle_info({tcp, Socket, Packet}, State = #state{gatewayProtocol = GatewayProtocol}) ->
    #state{
       selfPid = SelfPid,
       socket = Socket,
       gatewayId = GatewayId,
       transport = Transport,
       myTimeout = MyTimeout
      } = State,

    %% Cancel timer
    cancel_timer_of_mytimeout(MyTimeout),
    {ok, [_, {_ ,TIMER_TIME}, _, _]} = file:consult(?TIME_OPTS_FILEPATH),
    NewMyTimeout = #myTimeout{
                      numberOfTimeout = 0,
                      timerRef = mytimer:start_timer(TIMER_TIME, SelfPid, ?MY_TIMEOUT_MSG)
                     },

    ok = Transport:setopts(Socket, [{active, once}]),

    ?PRINT("recv ~p packet: ~p~n", [GatewayProtocol, hex_util:to_hex(Packet)]),
    StateTmp = handle_packet(GatewayProtocol, State, Packet),
    NewState = StateTmp#state{
                 myTimeout = NewMyTimeout
                },
    ?PRINT("LastPacket: ~p~n", [hex_util:to_hex(NewState#state.lastPacket)]),
    {noreply, NewState};

handle_info(Info, State) ->
    ?PRINT("~p~n", [Info]),
    {noreply, State}.

%%%=========================================================================
%% Internal functions
%%%=========================================================================

%%--------------------------------------------------------------------------
%% Handle packet
%%--------------------------------------------------------------------------
handle_packet(?PROTOCOL_3761, State, Packet) ->
    #state{
       gatewayId = GatewayId,
       socket = Socket,
       transport = Transport,
       lastPacket = LastPacket,
       activeReportPid = ActiveReportPid,
       clientSendRecvPid = ClientSendRecvPid
      } = State,
    
    %% Insert LastPacket in front of Packet
    NewPacket = binary:list_to_bin([LastPacket, Packet]),
    {NewLastPacket, FrameList} = parse_3761_packet(NewPacket),

    StateTmp = lists:foldl(fun handle_3761_packet/2, State, FrameList),
    
    StateTmp#state{
      lastPacket = NewLastPacket
     };
handle_packet(?PROTOCOL_645, State, Packet) ->
    #state{
       gatewayId = GatewayId,
       socket = Socket,
       transport = Transport,
       lastPacket = LastPacket,
       activeReportPid = ActiveReportPid,
       clientSendRecvPid = ClientSendRecvPid
      } = State,
    LastPacketTmp = 
        case Packet of
            <<16#FA, 16#01, 16#01, _/binary>> ->
                <<>>;
            _ ->
                LastPacket
        end,
    NewPacket = binary:list_to_bin([LastPacketTmp, Packet]),
    {NewLastPacket, DataFieldFlagAndFrameList} = parse_645_packet(NewPacket),
    %% Transmit frame list
    [transmit_645_frame(GatewayId, DataFieldFlagAndFrame, ActiveReportPid, ClientSendRecvPid) || 
        DataFieldFlagAndFrame <- DataFieldFlagAndFrameList],
    
    State#state{
      lastPacket = NewLastPacket
     };
handle_packet(GatewayProtocol, State, Packet) ->
    ?ERROR("Unknown protocol:~p~n", [GatewayProtocol]),
    State.


%%--------------------------------------------------------------------------
%% Call connector_active_report module functions
%%--------------------------------------------------------------------------

start_active_report_process(GatewayType, GatewayId, Parent, false) ->
    {ok, undefined};
start_active_report_process(GatewayType, GatewayId, Parent, true) ->
    {ok, _Pid} = connector_active_report:start_link([GatewayType, GatewayId, Parent]).

stop_active_report_process(Pid, shutdown) when is_pid(Pid) ->
    ok = connector_active_report:stop(Pid);
stop_active_report_process(Pid, _) when is_pid(Pid) ->
    ok = connector_active_report:gateway_offline(Pid);
stop_active_report_process(_, _) ->
    ok.

%%--------------------------------------------------------------------------
%% Call connector_client_send_recv module functions
%%--------------------------------------------------------------------------

start_client_send_recv_process(GatewayType, GatewayId, Parent) ->
    {ok, Pid} = connector_client_send_recv:start_link([GatewayType, GatewayId, Parent]),
    %% Save gateway => Clientsendrecvpid
    connector_gateway_to_store_server:insert_client_pid({GatewayType, GatewayId}, Pid),
    {ok, Pid}.

stop_client_send_recv_process(Pid, _) when is_pid(Pid) ->
    ok = connector_client_send_recv:stop(Pid);
stop_client_send_recv_process(_, _) ->
    ok.
    
%%--------------------------------------------------------------------------
%% gateway status
%%--------------------------------------------------------------------------

gateway_is_on_line(undefined) ->
    false;
gateway_is_on_line(_) ->
    true.


%%--------------------------------------------------------------------------
%% Parse packet, handle multiframe
%%--------------------------------------------------------------------------

parse_3761_packet(Packet) ->
    parse_3761_packet_(Packet, []).

parse_3761_packet_(Packet, FrameList) ->
    case binary:match(Packet, <<16#68>>, []) of
        {Start, Length = 1} ->
            case parse_3761_packet_result(Packet, Start) of
                {true, Frame} ->
                    [_, RestPacket] = binary:split(Packet, Frame, []),
                    parse_3761_packet_(RestPacket, [Frame | FrameList]);
                _ ->
                    {Packet, lists:reverse(FrameList)}
            end;
        _ ->
            {Packet, lists:reverse(FrameList)}
    end.

parse_3761_packet_result(Packet, Start) ->
    try {binary:at(Packet, Start+1+4) =:= 16#68, binary:part(Packet, Start+1, 4)} of
        {true, <<Len:16/little-integer, Len:16/little-integer>>} ->
            try {binary:part(Packet, Start+1+4+1, Len bsr 2), binary:part(Packet, Start+1+4+1+(Len bsr 2), 2)} of
                {<<Ctrl:8, _AddrBinary:5/binary-unit:8, Rest1/binary>> = DataFieldBinary, <<_Cs:8, 16#16>>} ->
                    Frame = binary:part(Packet, Start, 1+4+1+(Len bsr 2)+2),
                    {true, Frame};
                _ ->
                    false
            catch
                _:_ ->
                    false
            end;
        _ ->
            false
    catch 
        _Class:_Reason ->
            false
    end.


parse_645_packet(Packet) ->
    parse_645_packet_(Packet, []).


parse_645_packet_(Packet, DataFieldFlagAndFrameList) ->
    %% find frame begin flag: first hex "68"
    case {binary:match(Packet, <<16#EC>>, []), binary:match(Packet, <<16#ED>>, []), 
          binary:match(Packet, <<16#68>>, [])} of
        %% FA 01 01 EC ******* ED
        {{StartEC, 1}, {StartED, 1}, _} when 
              ((StartEC =:= 3) orelse (StartEC =:= 0)) 
              andalso ((StartEC+1 =:= StartED) orelse ((StartED-StartEC) rem 9 =:= 0)) -> 
            Frame = binary:part(Packet, StartEC, StartED-StartEC+1),
            [_, RestPacket] = binary:split(Packet, Frame, []),
            parse_645_packet_(RestPacket, [{<<>>, Frame} | DataFieldFlagAndFrameList]);
        {{StartEC, 1}, _, _} when (StartEC =:= 3) orelse (StartEC =:= 0) ->
            {Packet, lists:reverse(DataFieldFlagAndFrameList)};
        %% FA 01 01 ED OR ED
        {_, {StartED, 1}, _} when (StartED =:= 3) orelse (StartED =:= 0) ->
            Frame = binary:part(Packet, StartED, 1),
            [_, RestPacket] = binary:split(Packet, Frame, []),
            parse_645_packet_(RestPacket, [{<<>>, Frame} | DataFieldFlagAndFrameList]);
        {_, _, {Start68, Length = 1}} ->
            case parse_645_packet_result(Packet, Start68) of
                {true, DataFieldFlag, Frame} ->
                    [_, RestPacket] = binary:split(Packet, Frame, []),
                    parse_645_packet_(RestPacket, [{DataFieldFlag, Frame} | DataFieldFlagAndFrameList]);
                _ ->
                    {Packet, lists:reverse(DataFieldFlagAndFrameList)}
                    %[_, RestPacket] = binary:split(Packet, <<16#68>>, []),
                    %parse_packet_(RestPacket, DataFieldFlagAndFrameList)
            end;
        _ ->
            {Packet, lists:reverse(DataFieldFlagAndFrameList)}
    end.

parse_645_packet_result(Packet, Start) ->
    %% "68"(hex 1 byte) MeterId(6 bytes, 12 characters) "68"(find this hex "68")
    %% Skip hex "68"(1 byte) and MeterId(6 bytes)
    %% Skip hex "68"(1 byte) and control_code(1 byte), find length of data_field
    try {binary:at(Packet, Start+1+6) =:= 16#68, binary:at(Packet, Start+1+6+1+1)} of
        {true, DataFieldLengthTmp} when (DataFieldLengthTmp >= 0) ->
            %% Skip Datafieldlength(1 byte) and DataField(Datafieldlength bytes),
            %% find Cs and frame end flag: hex "16"
            %%=============================================================
            %% Skip hex "68"(1 byte) and control_code(1 byte), and datafieldlength(1 byte)
            %% File datafieldflag(4 bytes)
            DataFieldFlagTmp =
                try binary:part(Packet, Start+1+6+1+1+1, 4) of
                    DataFieldFlagTmp1 -> DataFieldFlagTmp1
                catch 
                    _:_ ->
                        <<>>
                end,
            ?PRINT("DataField ~s ~n", [hex_util:to_hex(DataFieldFlagTmp)]),
            DataFieldLength =
                try {lists:member(DataFieldFlagTmp, ?LONG_LENGTH_DATAFIELDFLAG_LIST), binary:at(Packet, Start+1+6+1+1+1+4)} of
                    {true, Length1Tmp} ->
                        ((Length1Tmp - 16#33) bsl 8) + DataFieldLengthTmp;
                    _ ->
                        DataFieldLengthTmp
                catch
                    _:_ ->
                        DataFieldLengthTmp
                end,
            ?PRINT("Len ~p~n", [DataFieldLength]),
            try binary:part(Packet, Start+1+6+1+1+1+DataFieldLength, 2) of
                <<Cs, 16#16>> ->
                    %% Calculate Cs
                    ?PRINT("Cs ~p~n", [Cs]),
                    try binary:part(Packet, Start, 1+6+1+1+1+DataFieldLength) of
                        DataTmpExCs16h ->
                            CsTmp = lists:sum(binary_to_list(DataTmpExCs16h)) rem 256,
                            case Cs =:= CsTmp of
                                true ->
                                    %%=============================================================
                                    %% One frame
                                    FrameTmp = binary:part(Packet, Start, 1+6+1+1+1+DataFieldLength+2),
                                    {true, DataFieldFlagTmp, FrameTmp};
                                false ->
                                    false
                            end
                    catch
                        _Class:_Reason ->
                            false
                    end;
                _ ->
                    false
            catch
                _Class:_Reason ->
                    false
            end;
        _ ->
            false
    catch
        _Class:_Reason ->
            false
    end.

%% eunit
-ifdef(EUNIT).

parse_645_packet_test() ->
    Frame1DataFieldFlag = <<16#34, 16#32, 16#FF, 16#EF>>,
    Frame1Hex = "682758012705166891183432FFEFA98335333756BA7833946C333232323483A8363AAD16",
    Packet1Hex = "FA0101" ++ Frame1Hex,
    Frame1Bin = hex_util:to_bin(Frame1Hex),
    Packet1Bin = hex_util:to_bin(Packet1Hex),

    Frame2DataFieldFlag = <<16#38, 16#32, 16#DD, 16#EF>>,
    Frame2Hex = "682632002312136836293832DDEF38C43733333333A835343353364A983333B4C43A33A8BB3C33564B7BC6336337364A5C4549E916",
    Packet2Hex = "FA0101" ++ Frame2Hex,
    Frame2Bin = hex_util:to_bin(Frame2Hex),
    Packet2Bin = hex_util:to_bin(Packet2Hex),

    Frame3DataFieldFlag = <<16#39, 16#3A, 16#0F, 16#EF>>,
    Frame3Hex = "68aaaaaaaaaaaa683692393A0FEF33793434353536353734383439353A343B343C343D353E343F35403541344" ++
        "23443354435453546354735483549354A354B354C354D354E354F3550355135523553355435553556355735583559355A345B355C" ++ 
        "355D345E345F3560356135623563356435653466346734683469356A356B356C3573347435753576357735783579357A357B347C34833" ++ 
        "4843585352216",
    Packet3Hex = "FA0101" ++ Frame3Hex,
    Frame3Bin = hex_util:to_bin(Frame3Hex),
    Packet3Bin = hex_util:to_bin(Packet3Hex),
    
    [
     ?assertMatch({<<>>, [{Frame1DataFieldFlag, Frame1Bin}]}, parse_645_packet(Frame1Bin)),
     ?assertMatch({<<>>, [{Frame1DataFieldFlag, Frame1Bin}]}, parse_645_packet(Packet1Bin)),
     ?assertMatch({<<>>, [{Frame2DataFieldFlag, Frame2Bin}]}, parse_645_packet(Frame2Bin)),
     ?assertMatch({<<>>, [{Frame2DataFieldFlag, Frame2Bin}]}, parse_645_packet(Packet2Bin)),
     ?assertMatch({<<>>, [{Frame1DataFieldFlag, Frame1Bin}, {Frame2DataFieldFlag, Frame2Bin}]},
                  parse_645_packet(binary:list_to_bin([Packet1Bin, Packet2Bin]))),
     ?assertMatch({<<16#12, 16#34, 16#56>>, [{Frame1DataFieldFlag, Frame1Bin}, {Frame2DataFieldFlag, Frame2Bin}]},
                  parse_645_packet(binary:list_to_bin([Packet1Bin, Packet2Bin, <<16#12, 16#34, 16#56>>]))),
     ?assertEqual(<<16#12>>, hex_util:to_bin("12")),
     ?assertEqual(<<16#12, 16#34>>, binary:list_to_bin([<<16#12>>, <<16#34>>])),
     ?assertMatch({<<>>, [{Frame3DataFieldFlag, Frame3Bin}]}, parse_645_packet(Frame3Bin)),
     ?assertMatch({<<>>, [{Frame3DataFieldFlag, Frame3Bin}]}, parse_645_packet(Packet3Bin)),

     ?assertEqual(1, 1)
    ].
    
-endif.

%%--------------------------------------------------------------------------
%% Transmit packet
%%--------------------------------------------------------------------------
    
transmit_645_frame(GatewayId, DataFieldFlagAndFrame, ActiveReportPid, ClientSendRecvPid) ->
    {DataFieldFlag, Frame} = DataFieldFlagAndFrame,
    ?PRINT("DataFieldFlag:~p~n", [hex_util:to_hex(DataFieldFlag)]),
    ?PRINT("Frame:~p~n", [hex_util:to_hex(Frame)]),
    case lists:member(DataFieldFlag, ?DATAMSG_DATAFIELDFLAG_LIST)
        orelse lists:member(DataFieldFlag, ?WARNMSG_DATAFIELDFLAG_LIST) 
        orelse lists:member(DataFieldFlag, ?CONTROL_DATAFIELDFLAG_LIST) of
        true ->
            %% DataFieldFlag is active report
            connector_active_report:active_report_645_packet(ActiveReportPid, Frame);
        false ->
            %% DataFieldFlag isn't active report
            %% TODO

            %% Log record: gateway order recv log
            connector_event_server:gateway_order_recv_log(GatewayId, hex_util:to_hex(Frame)),
            
            %% Send to client_send_recv process
            connector_client_send_recv:packet_from_gateway(ClientSendRecvPid, Frame),
            ok
    end.


%%--------------------------------------------------------------------------
%% 3761 util functions
%%--------------------------------------------------------------------------

%% 处理完整3761的一帧
handle_3761_packet(<<16#68, Len:16/little-integer, Len:16/little-integer, 16#68, Rest/binary>> = Packet, State) ->
    #state{
       selfPid = SelfPid,
       gatewayProtocol = GatewayProtocol,
       socket = Socket,
       transport = Transport,
       lastPacket = _LastPacket,
       activeReportPid = ActiveReportPid,
       clientSendRecvPid = ClientSendRecvPid,
       myTimeout = MyTimeout
      } = State,

    FrameHex = hex_util:to_hex(Packet),
    ?PRINT("3761 protocol packet: ~p~n", [FrameHex]),

    <<Ctrl:8, AddrBinary:5/binary-unit:8, Rest1/binary>> = Rest,

    GatewayId = gen_concentrator_gateway_id(AddrBinary),
    ?PRINT("GatewayId: ~p~n", [GatewayId]),

    %% 更新网关对应进程Id及PFC
    case connector_gateway_to_store_server:lookup_gateway_pid(GatewayId) of
        {ok, OldPid} when (OldPid =/= SelfPid) ->
            ?MODULE:shutdown(OldPid);
        {ok, SelfPid} ->
            ok;
        {error, not_found} ->
            connector_gateway_to_store_server:insert_gateway_pid(GatewayId, SelfPid),
            connector_gateway_to_store_server:init_pfc(GatewayId)
    end,
    
    StateTmp = 
        case {is_set_prm_bit(Ctrl), Rest1} of
            {true,  <<AFN:8, Seq:8, DA1:8, DA2:8, DT1:8, DT2:8, _/binary>>} ->
                ResponFrame = gen_respon_frame(AddrBinary, Seq),
                ok = Transport:send(Socket, ResponFrame),
                ?PRINT("Concentrator respon frame:~p~n", [hex_util:to_hex(ResponFrame)]),
                
                {ok, GatewayType} = connector_db_store_server:get_gateway_type(GatewayId),

                Pn = gen_pn(DA1, DA2),
                Fn = gen_fn(DT1, DT2),
                case {is_login(AFN, Pn, Fn), is_heartbeat(AFN, Pn, Fn)} of
                    {false, false} ->
                        %% Set prm bit neither login nor heartbeat
                        
                        connector_active_report:active_report_3761_packet(ActiveReportPid, Packet),
                        ?PRINT("Concentrator:~p is report: ~p ~n", [GatewayId, FrameHex]),
                        State;
                    {IsLogin, IsHeartBeat} when (IsLogin =:= true) orelse (IsHeartBeat =:= true) ->
                        case IsLogin of
                            true ->
                                %% Log record: gateway login log
                                {ok, {Ip, Port}} = Transport:peername(Socket),   
                                %% Log record: gateway login log
                                Status = "01",
                                connector_event_server:gateway_status_log(GatewayId, Status),
                                gateway_status_store(GatewayId, ?ONLINE_STATUS),
                                case inet:ntoa(Ip) of
                                    {error, _} ->
                                        connector_event_server:gateway_login_log(GatewayId, FrameHex);
                                    Address ->
                                        Msg = string:join([Address, FrameHex], " "),
                                        ?PRINT("Address:~p~n", [Msg]),
                                        connector_event_server:gateway_login_log(GatewayId, Msg)
                                end,
                                ?PRINT("Concentrator:~p is login~n", [GatewayId]),
                                connector_active_report:login_report_3761(GatewayType, GatewayId),
                                ok;
                            false ->
                                %% Log record: gateway heartbeat log
                                update_gateway_status(GatewayId, ?ONLINE_STATUS),
                                connector_event_server:gateway_heart_beat_log(GatewayId, FrameHex),
                                ?PRINT("Concentrator:~p is heartbeat~n", [GatewayId])
                        end,
                                               
                        case gateway_is_on_line(GatewayProtocol) of
                            false ->
                                %% Create active report process and client send/recv process
                                %% TODO
                                ReportFlag = true,
                                {ok, NewActiveReportPid} = start_active_report_process(GatewayType, GatewayId, SelfPid, ReportFlag),
                                {ok, NewClientSendRecvPid} = start_client_send_recv_process(GatewayType, GatewayId, SelfPid),
                                %% Gateway online need to active report
                                connector_active_report:active_report_3761_packet(ActiveReportPid, Packet),
                                State#state{
                                  activeReportPid = NewActiveReportPid,
                                  clientSendRecvPid = NewClientSendRecvPid,
                                  gatewayType = GatewayType,
                                  gatewayProtocol = ?PROTOCOL_3761
                                 };
                            true ->
                                State
                        end
                end;
            _ ->
                %% Not set prm bit
                %% Send to client_send_recv process
                connector_client_send_recv:packet_from_gateway(ClientSendRecvPid, Packet),

                %% Log record: gateway order recv log
                connector_event_server:gateway_order_recv_log(GatewayId, hex_util:to_hex(Packet)),
                State
        end,
    StateTmp#state{gatewayId = GatewayId}.

%% Generate concentrator addr binary from gatewayId
gen_concentrator_addr_binary(GatewayId) ->
    AddrStr = string:right(GatewayId, 9),
    {Addr1, Addr2} = lists:split(4, AddrStr),
    Addr2Integer = list_to_integer(Addr2, 10),
    <<A1L, A1H>> = hex_util:to_bin(Addr1),
    A1 = <<A1H, A1L>>,
    <<A2L, A2H>> = <<Addr2Integer:16>>,
    A2 = <<A2H, A2L>>,
    A3 = <<16#00>>,
    <<A1/binary, A2/binary, A3/binary>>. 

%% Generate gateway id of concentrator 
gen_concentrator_gateway_id(<<A1L, A1H, A2:16/little, A3>>) ->
    A1LStr = lists:flatten(io_lib:format("~2..0s", [hex_util:to_hex(A1L)])),
    A1HStr = lists:flatten(io_lib:format("~2..0s", [hex_util:to_hex(A1H)])),
    A2Str = lists:flatten(io_lib:format("~5..0w", [A2])),
    lists:concat([A1HStr, A1LStr, A2Str]).

is_set_prm_bit(Ctrl) ->
    Mask = 1 bsl 6,
    (Ctrl band Mask) =:= Mask.

is_login(16#02, 0, 1) ->
    true;
is_login(_, _, _) ->
    false.

is_heartbeat(16#02, 0, 3) ->
    true;
is_heartbeat(_, _, _) ->
    false.

gen_respon_frame(AddrBinary, PSeq) ->
    Ctrl = 16#0B,
    AFN = 16#00,
    RSeq = PSeq,
    Pn = 0,
    Fn = 1,
    DAUnitBinary = gen_da_unit_binary(Pn),
    DTUnitBinary = gen_dt_unit_binary(Fn),
    DataFieldBinary = <<Ctrl:8, AddrBinary/binary, AFN:8, RSeq:8, DAUnitBinary/binary, DTUnitBinary/binary>>,
    Len1 = byte_size(DataFieldBinary),
    Len = (Len1 bsl 2) bor 2#10,
    Cs = gen_cs(DataFieldBinary),
    <<16#68, Len:16/little, Len:16/little, 16#68, DataFieldBinary/binary, Cs:8, 16#16>>.

gen_cs(DataFieldBinary) when is_binary(DataFieldBinary) ->
    gen_cs(binary_to_list(DataFieldBinary));
gen_cs(DataFieldList) when is_list(DataFieldList) ->
    lists:sum(DataFieldList) band 16#FF.

gen_index(1) -> 1;
gen_index(2) -> 2;
gen_index(4) -> 3;
gen_index(8) -> 4;
gen_index(16) -> 5;
gen_index(32) -> 6;
gen_index(64) -> 7;
gen_index(128) -> 8.

gen_pn(0, 0) ->
    0;
gen_pn(16#FF, 0) ->
    %% 所有有效测量点(不包含Pn=0)
    0;
gen_pn(DA1, DA2) when (DA1 > 0) andalso (DA2 > 0) ->
    (DA2 - 1) * 8 + gen_index(DA1).

gen_fn(DT1, DT2) when (DT1 > 0) andalso (DT2 >= 0) ->
    (DT2 * 8) + gen_index(DT1).
    
gen_da_unit_binary(0) ->
    <<0:8, 0:8>>;
gen_da_unit_binary(Pn) when (Pn >= 1) andalso (Pn =< 2040) ->
    DA2 = (Pn + 7) div 8,
    DA1 = 1 bsl ((Pn - 1) rem 8),
    <<DA1:8, DA2:8>>.
    
gen_dt_unit_binary(Fn) when (Fn >= 1) andalso (Fn =< 248) ->
    DT2 = (Fn - 1) div 8, 
    DT1 = 1 bsl ((Fn - 1) rem 8),
    <<DT1:8, DT2:8>>.

%% Eunit
-ifdef(EUNIT).

gen_pn_test() ->
    [
     ?assertEqual(0, gen_pn(0, 0)),
     ?assertEqual(1, gen_pn(1, 1)),
     ?assertEqual(24, gen_pn(128, 3)),
     ?assertEqual(2033, gen_pn(1, 255))
    ].

gen_fn_test() ->
    [
     ?assertEqual(1, gen_fn(1, 0)),
     ?assertEqual(24, gen_fn(128, 2)),
     ?assertEqual(241, gen_fn(1, 30))
    ].

gen_da_unit_binary_test() ->
    [
     ?assertEqual(<<0, 0>>, gen_da_unit_binary(0)),
     ?assertEqual(<<1, 1>>, gen_da_unit_binary(1)),
     ?assertEqual(<<128, 2>>, gen_da_unit_binary(16)),
     ?assertEqual(<<1, 255>>, gen_da_unit_binary(2033))
    ].

gen_dt_unit_binary_test() ->
    [
     ?assertEqual(<<1, 0>>, gen_dt_unit_binary(1)),
     ?assertEqual(<<128, 2>>, gen_dt_unit_binary(24)),
     ?assertEqual(<<1, 30>>, gen_dt_unit_binary(241))
    ].

-endif.

%%--------------------------------------------------------------------------
%% Cancel timer of myTimeout record
%%--------------------------------------------------------------------------

cancel_timer_of_mytimeout(MyTimeout) when is_record(MyTimeout, myTimeout) ->
    TimerRef = MyTimeout#myTimeout.timerRef,
    mytimer:cancel_timer(TimerRef);
cancel_timer_of_mytimeout(_) ->    
    ok.

update_gateway_status(GatewayId, Status) ->
    Time = helper_util:datetime_string(),
    case connector_gateway_status_store:lookup(GatewayId) of
        ok ->
          connector_gateway_status_store:insert(GatewayId, Status, Time);
        {error, not_found} ->
            ok
    end.

gateway_status_store(GatewayId, Status) ->
    Time = helper_util:datetime_string(), 
    connector_gateway_status_store:insert(GatewayId, Status, Time).
