-module(connector_rest_handler).

-include("print.hrl").
-include("config.hrl").

-export([init/2]).
-export([allowed_methods/2]).
-export([allow_miss_port/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).

-define(SUCCESS_STATUS, <<"200">>).
-define(FAILURE_STATUS, <<"300">>).

-export([
         response_to_json/2, 
         response_to_html/2, 
         response_to_text/2
        ]).

-export([
         get_status/1,
         get_version/1,
         get_network/1,
         update_network/1,
         restart_network/1,
         hello/1
        ]).

init(Req, State) ->
    ?PRINT("REQ: ~p~n", [Req]),
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    Result = [<<"GET">>, <<"POST">>, <<"HEAD">>, <<"OPTIONS">>],
    {Result, Req, State}.

allow_miss_port(Req, State) ->
    Result = false,
    {Result, Req, State}.

content_types_provided(Req, State) ->
    Result = [
              %{<<"text/html">>,  response_to_html},
              %{<<"text/plain">>, response_to_text},
              %{<<"application/x-www-form-urlencoded">>, response_to_json},
              {<<"application/json">>,  response_to_json}
             ],
    {Result, Req, State}.

content_types_accepted(Req, State) ->
    Result = [
              {<<"application/x-www-form-urlencoded">>, response_to_json},
              {<<"application/json">>,  response_to_json}
             ],
    {Result, Req, State}.

response_to_json(Req, State) ->
    Path = cowboy_req:path(Req),
    Method = cowboy_req:method(Req),
    {NewReq, Qs} = 
        case cowboy_req:has_body(Req) of
            true ->
                {ok, Body, ReqTmp} = cowboy_req:read_body(Req),
                ?PRINT("Body: ~p~n", [Body]),
                {ReqTmp, jsx:decode(Body)};
            false ->
                {Req, cowboy_req:parse_qs(Req)}
        end,

    Result = get_result(Path, Qs),
   % ?PRINT("Result:~s~n", [binary_to_list(Result)]),

   % ?PRINT("Pid: ~p~n", [self()]),
    ?PRINT("Path: ~p~n", [Path]),
    ?PRINT("Qs: ~p~n", [Qs]),

    case Method of
        <<"POST">> ->
            Res1 = cowboy_req:set_resp_body(Result, NewReq),
            Res2 = cowboy_req:delete_resp_header(<<"content-type">>, Res1),
            Res3 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res2),
            {true, Res3, State};
        _ ->
            {Result, NewReq, State}
    end.

response_to_html(Req, State) ->
    Result = <<"<html>",
               "<body>",
               "<p>REST Hello World as HTML!</p>",
               "</body>",
               "</html>">>,
    {Result, Req, State}.

response_to_text(Req, State) ->
    Result = <<"Hello World!">>,
    {Result, Req, State}.

get_result(Path, Qs) ->
    Fun = proplists:get_value(filename:basename(Path), ?PATH_TO_FUN, hello),
    ?PRINT("Fun: ~p~n", [Fun]),
    apply(?MODULE, Fun, [Qs]).

hello(Qs) ->
    Result = <<"{\"returnCode\":\"400\", \"returnMsg\": \"Hello World!\"}">>.

format_return(Reason, ReturnCode) ->
    ReturnMsg = ?HELPER:to_iolist(Reason),
    jsx:encode([{<<"returnCode">>, ReturnCode}, {<<"returnMsg">>, ReturnMsg}]).

data_return(Data) ->
    jsx:encode([{<<"returnCode">>, ?SUCCESS_STATUS}, {<<"data">>, Data}]).

get_status(_Qs) ->
    Data =
        case connector_gateway_status_store:show() of
          {ok, Info} ->
               io:format("Info:~p~n",[Info]),
               [[{<<"gatewayid">>, helper_util:to_iolist(GatewayCode)}, {<<"status">>, helper_util:to_iolist(Status)}, {<<"time">>, helper_util:to_iolist(Time)}] || {GatewayCode, Status, Time} <- Info];
          _ ->
                "have no gateway"
        end,
    data_return(Data).

get_version(_Qs) ->
    Vsn = 
        case application:get_env(connector, version) of
            {ok, VsnTmp} ->
                VsnTmp;
        _ ->
                "unknown version"
        end,
    DateStr =
        case application:get_env(connector, date) of
            {ok, DateTmp} ->
                DateTmp;
        _ ->
                "unknown release date"
        end,
    List = [{<<"version">>, helper_util:to_iolist(Vsn)}, {<<"date">>, helper_util:to_iolist(DateStr)}],
    data_return(List).

get_network(_Qs) ->
    case get_network_config() of
        {ok, ConfigList} ->
            Ip = helper_util:to_iolist(proplists:get_value(?IPADDR, ConfigList, "")),
            NetMask = helper_util:to_iolist(proplists:get_value(?NETMASK, ConfigList, "")),
            Gateway = helper_util:to_iolist(proplists:get_value(?GATEWAY, ConfigList, "")),
            Dns1 = helper_util:to_iolist(proplists:get_value(?DNS1, ConfigList, "")),
            List = [{<<"ip">>, Ip}, {<<"netmask">>, NetMask}, {<<"gateway">>, Gateway}, {<<"dns1">>, Dns1}],
            data_return(List);
        {error, Reason} ->
            format_return(Reason, ?FAILURE_STATUS)
    end.

get_network_config() ->
    case file:read_file(?NETWORK_CONFIG_FILE) of
        {ok, Binary} ->
            DataList = string:tokens(binary_to_list(Binary), "\n"),
            ?PRINT("DataList:~p~n", [DataList]),
            Fun = 
                fun(DataLine, List) ->
                        case string:tokens(DataLine, "=") of
                            [Key, Value] when (Key =:= ?IPADDR) orelse (Key =:= ?NETMASK) 
                                              orelse (Key =:= ?GATEWAY) orelse (Key =:= ?DNS1) ->
                                [{Key, Value} | List];
                            _ ->
                                List
                        end
                end,
            ConfigList = lists:foldl(Fun, [], DataList),
            {ok, ConfigList};
        {error, Reason} ->
            {error, Reason}
    end.

update_network(Qs) ->
    List = [{<<"ip">>, "-a"}, {<<"netmask">>, "-b"}, {<<"gateway">>, "-c"}, {<<"dns1">>, "-d"}],
    Fun = 
        fun({Param, Opt}, ParamStr) ->
           case proplists:get_value(Param, Qs) of
               undefined ->
                   ParamStr;
               ValueBin ->
                   ParamStr ++ Opt ++ " " ++  binary_to_list(ValueBin) ++ " "
           end
        end,
    ParamStr = lists:foldl(Fun, "", List),
    ref_util:update_network(ParamStr),
    ReturnMsg = "update success",
    ReturnCode = ?SUCCESS_STATUS,
    format_return(ReturnMsg, ReturnCode).
    
restart_network(_Qs) ->
    ParamStr = "systemctl restart network.service",
    Fun = 
        fun(ParamStr) ->
                timer:sleep(1000),
                ref_util:seteuid(ParamStr)
        end,
    spawn(fun() -> Fun(ParamStr) end),
    ReturnMsg = "restart success",
    ReturnCode = ?SUCCESS_STATUS,
    format_return(ReturnMsg, ReturnCode).

