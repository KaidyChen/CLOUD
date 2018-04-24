-module(connector_event_server).

-behaivor(gen_event).

%% API
-export([
         start_link/0,
         add_handler/2,
         delete_handler/2
]).
-export([
         active_report_log/2,
         gateway_login_log/2,
         gateway_heart_beat_log/2,
         gateway_status_log/2,
         operation_log/2,
         gateway_order_send_log/2,
         gateway_order_recv_log/2,
         request_log/2,
         response_log/2
]).

-define(SERVER, ?MODULE).

start_link() ->
    {ok, Pid} = gen_event:start_link({local, ?SERVER}),
    %% Add event handler
    add_handler(),
    {ok, Pid}.

%% Add event handler
add_handler() ->
    connector_event_gateway_log:add_handler(),
    connector_event_active_report_log:add_handler(),
    connector_event_gateway_send_recv_log:add_handler(),
    connector_event_request_response_log:add_handler(),
    connector_event_operation_log:add_handler(),
    ok.   

add_handler(Handler, Args) ->
    gen_event:add_handler(?SERVER, Handler, Args).

delete_handler(Handler, Args) ->
    gen_event:delete_handler(?SERVER, Handler, Args).

active_report_log(MeterId, Msg) ->
    gen_event:notify(?SERVER, {active_report_log, MeterId, Msg}).

gateway_login_log(GatewayId, Msg) ->
    gen_event:notify(?SERVER, {gateway_login_log, GatewayId, Msg}).

gateway_heart_beat_log(GatewayId, Msg) ->
    gen_event:notify(?SERVER, {gateway_heart_beat_log, GatewayId, Msg}).

gateway_status_log(GatewayId, Msg) ->
    gen_event:notify(?SERVER, {gateway_status_log, GatewayId, Msg}).

gateway_order_send_log(GatewayId, Msg) ->
    gen_event:notify(?SERVER, {gateway_order_send_log, GatewayId, Msg}).

gateway_order_recv_log(GatewayId, Msg) ->
    gen_event:notify(?SERVER, {gateway_order_recv_log, GatewayId, Msg}).

request_log(OrderNumber, Msg) ->
    gen_event:notify(?SERVER, {request_log, OrderNumber, Msg}).

response_log(OrderNumber, Msg) ->
    gen_event:notify(?SERVER, {response_log, OrderNumber, Msg}).

operation_log(MeterId, Msg) ->
    gen_event:notify(?SERVER, {operation_log, MeterId, Msg}).

