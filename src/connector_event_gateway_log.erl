-module(connector_event_gateway_log).

-include("config.hrl").
-include("log.hrl").
-include("print.hrl").

%% API
-export([
         add_handler/0,
         delete_handler/0
]).

%% Gen_event callbacks
-export([
         init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         terminate/2,
         code_change/3
]).

-define(HANDLER, ?MODULE).

-record(state, {}).

add_handler() ->
    connector_event_server:add_handler(?HANDLER, []).

delete_handler() ->
    connector_event_server:delete_handler(?HANDLER, []).

init([]) ->
    {ok, #state{}, hibernate}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Arg, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_event({gateway_login_log, GatewayId, Msg}, State) when is_list(GatewayId) ->
    Datetime = ?HELPER:datetime(),
    LogFile = log_util:get_gateway_log_file(Datetime),
    MsgType = ?LOGIN_MSG_TYPE,
    Content = log_util:gen_content(Datetime, MsgType, GatewayId, Msg),
    log_util:write_log(LogFile, Content),
    {ok, State, hibernate};    
handle_event({gateway_heart_beat_log, GatewayId, Msg}, State) when is_list(GatewayId) ->
    Datetime = ?HELPER:datetime(),
    LogFile = log_util:get_gateway_log_file(Datetime),
    MsgType = ?HEART_BEAT_MSG_TYPE,
    Content = log_util:gen_content(Datetime, MsgType, GatewayId, Msg),
    log_util:write_log(LogFile, Content),
    {ok, State, hibernate};    
handle_event({gateway_status_log, GatewayId, Msg}, State) when is_list(GatewayId) ->
    Datetime = ?HELPER:datetime(),
    LogFile = log_util:get_gateway_log_file(Datetime),
    MsgType = ?GATEWAY_STATUS_TYPE,
    Content = log_util:gen_content(Datetime, MsgType, GatewayId, Msg),
    log_util:write_log(LogFile, Content),
    {ok, State, hibernate};
handle_event(_Event, State) ->
    {ok, State, hibernate}.
