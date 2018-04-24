-module(connector_cloud_socket).

-behaviour(gen_server).

-include("config.hrl").
-include("print.hrl").

%% Public API
-export([start/0]).  

%% gen_server callback API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(CONNECT_OPTS, [binary, {active, once}, {packet, raw}]).
-define(TIMEOUT, 60*1000).

-record(state, {socket}).

start() ->
    [{cloudproxy_request_opts, CloudOpts}] = ets:lookup(?LISTEN_OPTS_TABLE, cloudproxy_request_opts),
    case proplists:get_value(cloudswitch, CloudOpts) of
        "on" ->
            start_link();
        "off" ->
            ok
    end.

start_link() ->
    proc_lib:start_link(?MODULE, init, [[]]).

init([]) ->
    ok = proc_lib:init_ack({ok, self()}),
    Packet = get_login_msg(),
    Socket = login(Packet),
    State = #state{socket=Socket},
    gen_server:enter_loop(?MODULE, [], State, ?TIMEOUT).

handle_cast(_Msg, State) ->  
    {noreply, State}. 

handle_call(_Request, _From, State) -> 
    {noreply, State}.  

terminate(_Reason, State) ->
    gen_tcp:close(State#state.socket),
    ok. 

code_change(_OldVersion, State, _Extra) -> 
    {ok, State}. 

handle_info({tcp_closed, _Socket}, State) ->
    ?PRINT("tcp_closed!!!~n",[]),
    {stop, normal, State};
handle_info({tcp_error, _Socket, Reason}, State) ->
    ?PRINT("tcp_error!!!~n", []),
    {stop, normal, State};
handle_info({tcp, Socket, DataBin}, State) ->
    ok = inet:setopts(Socket, [{active,once}]),
    ?PRINT("Data:~p~n",[DataBin]),
    [Method, Data]= string:tokens(binary_to_list(DataBin),"#"),
    case Method of
        "post" ->
            case data_process_post(list_to_binary(Data)) of
                {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
                    Body;
                {error, _Reason} ->
                    Body = [{400, [], <<"Bad request">>}],
                    Body 
            end;
        "get" ->
            case data_process_get(list_to_binary(Data)) of
                {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
                    Body;
                {error, _Reason} ->
                    Body = [{400, [], <<"Bad request">>}],
                    Body
            end;
         "head" ->
            case data_process_head(list_to_binary(Data)) of
                {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
                    Body;
                {error, _Reason} ->
                    Body = [{400, [], <<"Bad request">>}],
                    Body
            end
    end,
    ResponseBin = list_to_binary(Body),
    case gen_tcp:send(Socket, ResponseBin) of
        ok ->
            ?PRINT("response success!!!~n",[]);
        {error, Reason} ->
            ?PRINT("response fail!!!~n",[])
    end,
    {noreply, State, ?TIMEOUT};    
handle_info({tcp, Socket, _Info}, State) ->
    ok = inet:setopts(Socket, [{active,once}]),
    {noreply, State, ?TIMEOUT};
handle_info(timeout, State) ->
    #state{socket = Socket} = State,
    Packet = get_login_msg(),
    case gen_tcp:send(Socket, Packet) of
        ok ->
            {noreply, State, ?TIMEOUT};
        {error, _Reason} ->
            NewSocket = login(Packet),
            NewState = State#state{socket=NewSocket},
            {noreply, NewState, ?TIMEOUT}
    end.

%%手动构造模拟登录报
get_login_msg() ->
    {ok, Terms} = file:consult(?COMPUTER_OPTS_FILEPATH),
    [{computer_opt, Address}] = Terms,
    LoginStr = "FA071302FA01"++ Address ++ "FAFF",
    LoginPacket = ?HELP:string_to_binary(LoginStr),
    LoginPacket.

login(Data) ->
    [{cloudproxy_request_opts, CloudOpts}] = ets:lookup(?LISTEN_OPTS_TABLE, cloudproxy_request_opts),
    Ip = proplists:get_value(ip, CloudOpts),
    Port = proplists:get_value(port, CloudOpts),
    {ok, Socket} = gen_tcp:connect(Ip, Port, ?CONNECT_OPTS),
    case gen_tcp:send(Socket, Data) of
        ok ->
            ?PRINT("send success!!!~n",[]);
        {error, Reason} ->
            ?PRINT("send error:~p~n",[Reason])
    end,
    Socket.

data_process_post(DataBin) ->
    Url = get_url() ++ "/",
    application:start(inets),
    ?PRINT("Url:~p~n",[Url]),
    httpc:request(post, {Url, [], "application/x-www-form-urlencoded", DataBin}, [{timeout, 300000}, {connect_timeout, 300000}], []).

data_process_get(DataBin) ->
    ?PRINT("DataBin:~p~n",[DataBin]),
    Url = get_url() ++ binary_to_list(DataBin),
    application:start(inets),
    ?PRINT("Url:~p~n",[Url]),
    httpc:request(Url).

data_process_head(DataBin) ->
    [Path, RequestContent] = string:tokens(binary_to_list(DataBin), "@"),
    Url = get_url() ++ Path,
    RequestContentBin = list_to_binary(RequestContent),
    application:start(inets),
    ?PRINT("Url:~p~n",[Url]),
    httpc:request(post, {Url, [], "application/x-www-form-urlencoded", RequestContentBin}, [{timeout, 300000}, {connect_timeout, 300000}], []).

get_url() ->
    [{client_request_opts, ClientRequestOpts}] = ets:lookup(?LISTEN_OPTS_TABLE, client_request_opts),
    [{_, Port}, _] = proplists:get_value(transOpts, ClientRequestOpts, []),
    Url = "http://127.0.0.1" ++ ":" ++ integer_to_list(Port),
    Url.
