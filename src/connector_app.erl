-module(connector_app).

-behaviour(application).

-include("config.hrl").
-include("db.hrl").
-include("print.hrl").

%% Application callbacks
-export([
         start/2,
         stop/1
        ]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    {ok, Sup} = connector_sup:start_link(),
    load_config(),
    %% Start servers
    connector_gateway_status_store:init(),
    start_servers(Sup),
    {ok, Sup}.

stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

load_config() ->
    %load_env(),
    load_listen_opts(),
    ok.

load_env() ->
    {ok, Terms} = file:consult(?ENV_OPTS_FILEPATH),
    Fun = 
        fun({VarName, Value}) ->
                OldValue = 
                    case os:getenv(VarName) of
                        false ->
                            "";
                        OldValueTmp ->
                            OldValueTmp
                    end,
                os:unsetenv(VarName)
%                os:putenv(VarName, string:join([OldValue, filename:absname(Value)], ":"))
        end,
    lists:foreach(Fun, Terms).
                
load_listen_opts() ->
    ?LISTEN_OPTS_TABLE = ets:new(?LISTEN_OPTS_TABLE, [set, public, named_table]),
    {ok, Terms} = file:consult(?LISTEN_OPTS_FILEPATH),
    ets:insert(?LISTEN_OPTS_TABLE, Terms),
    ok.

start_servers(Sup) ->
    Servers = [
               {"Database server", fun start_db/0},
               {"Db store server", connector_db_store_server},
               {"Gateway to store server", connector_gateway_to_store_server},
               {"Connector event server", connector_event_server},
               {"connector task server", fun start_task/0},
               {"connector_cloud_socket", fun start_cloud/0},
               {"Tcp server", fun start_tcp/0},
               {"Http server", fun start_http/0}
              ],
    [start_server(Sup, Server) || Server <- Servers].

start_server(_Sup, {Name, Fun}) when is_function(Fun) ->
    ?INFO("~s is starting...", [Name]),
    Fun(),
    ?INFO_MSG("[done]");
start_server(Sup, {Name, Server}) ->
    ?INFO("~s is starting...", [Name]),
    app_util:start_child(Sup, Server),
    ?INFO_MSG("[done]");
start_server(Sup, {Name, Server, Opts}) ->
    ?INFO("~s is starting...", [Name]),
    app_util:start_child(Sup, Server, Opts),
    ?INFO_MSG("[done]");
start_server(_, _) ->
    erlang:error(badarg).

start_tcp() ->
    [{gateway_connect_opts, GatewayConnectOpts}] = ets:lookup(?LISTEN_OPTS_TABLE, gateway_connect_opts),
    NbAcceptors = proplists:get_value(nbAcceptors, GatewayConnectOpts, 100),
    TransOpts = proplists:get_value(transOpts, GatewayConnectOpts, []),
    {ok, _} = ranch:start_listener(connector_channel, NbAcceptors,
                                    ranch_tcp, TransOpts,
                                    connector_channel_protocol, []),
    ?INFO("GatewayConnectOpts: ~p", [GatewayConnectOpts]),
    ok.

start_http() ->
    Dispatch = cowboy_router:compile([
                                     {
                                       '_', [
                                             {"/", request_handler, []},
                                             {"/Action", request_handler, []},
                                             {"/connector/[...]", connector_rest_handler, []},
                                             {"/dataBase/[...]", db_rest_handler, []}
                                            ]
                                     }
                                    ]),
    [{client_request_opts, ClientRequestOpts}] = ets:lookup(?LISTEN_OPTS_TABLE, client_request_opts),
    NbAcceptors = proplists:get_value(nbAcceptors, ClientRequestOpts, 100),
    TransOpts = proplists:get_value(transOpts, ClientRequestOpts, []),
    ProtoOptsList = proplists:get_value(protoOpts, ClientRequestOpts, []),
    %RequestTimeout = proplists:get_value(request_timeout, ClientRequestOpts, 30000),
    %MaxKeepalive = proplists:get_value(max_keepalive, ClientRequestOpts, 100),
    ProtoOptsTmp = #{
      env => #{dispatch => Dispatch}
      %request_timeout => RequestTimeout,
      %max_keepalive => MaxKeepalive
     },
    ProtoOpts = maps:merge(ProtoOptsTmp, maps:from_list(ProtoOptsList)),
    {ok, _} = cowboy:start_clear(client_request, NbAcceptors, TransOpts, ProtoOpts),
    ?INFO("ClientRequestOpts: ~p", [ClientRequestOpts]),
    ok.

start_db() ->
    {ok, Terms} = file:consult(?DB_CONFIG_FILEPATH),
    [{db_opts, Options}] = Terms,
    DbName = ?DBNAME,
    sqlite3:start_link(DbName, Options).

start_task() ->
    connector_task_server:start().

start_cloud() ->
    connector_cloud_socket:start().



















