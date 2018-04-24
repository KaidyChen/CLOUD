-module(connector_gateway_to_store_server).

-behaivor(gen_server).

-export([start_link/0]).
-export([
         insert_gateway_pid/2,       
         delete_gateway_pid/1,
         lookup_gateway_pid/1,
         insert_client_pid/2,
         delete_client_pid/1,
         lookup_client_pid/1,
         init_pfc/1,
         get_pfc/1
        ]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-record(state, {
         }).

-define(SERVER, ?MODULE).

-define(GATEWAY_TO_GATEWAY_PID_TAB, gateway_to_gateway_pid).
-define(GATEWAY_TO_CLIENT_PID_TAB, gateway_to_client_pid).
-define(GATEWAY_TO_PFC_TAB, gateway_to_pfc).

%%%===========================================================================
%% API
%%%===========================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Gateway Pid
insert_gateway_pid(GatewayId, Pid) ->
    gen_server:cast(?SERVER, {insert_store, ?GATEWAY_TO_GATEWAY_PID_TAB, GatewayId, Pid}).
delete_gateway_pid(GatewayId) ->
    gen_server:cast(?SERVER, {delete_store, ?GATEWAY_TO_GATEWAY_PID_TAB, GatewayId}).
lookup_gateway_pid(GatewayId) ->
    lookup_for_store(?GATEWAY_TO_GATEWAY_PID_TAB, GatewayId).


%% Client Pid
insert_client_pid({GatewayType, GatewayId} = Key, ClientPid) ->
    gen_server:cast(?SERVER, {insert_store, ?GATEWAY_TO_CLIENT_PID_TAB, Key, ClientPid}).
delete_client_pid({GatewayType, GatewayId} = Key) ->
    gen_server:cast(?SERVER, {delete_store, ?GATEWAY_TO_CLIENT_PID_TAB, Key}).
lookup_client_pid({GatewayType, GatewayId} = Key) ->
    lookup_for_store(?GATEWAY_TO_CLIENT_PID_TAB, Key).

%% PFC
init_pfc(GatewayId) ->
    gen_server:cast(?SERVER, {init_pfc, GatewayId}).
get_pfc(GatewayId) ->
    gen_server:call(?SERVER, {get_pfc, GatewayId}).

%%%============================================================================
%% Gen_server callbacks
%%%============================================================================

init([]) ->
    EtsList = [?GATEWAY_TO_GATEWAY_PID_TAB, ?GATEWAY_TO_CLIENT_PID_TAB, ?GATEWAY_TO_PFC_TAB], 
    ets_init(EtsList),
    {ok, #state{}}.

handle_call({get_pfc, GatewayId}, _From, State) ->
    Reply = 
        try ets:update_counter(?GATEWAY_TO_PFC_TAB, GatewayId, {2, 1, 255, 0}) of
            PFC ->
                PFC
        catch
            _:_ ->
                0
        end,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({init_pfc, GatewayId}, State) ->
    insert_store(?GATEWAY_TO_PFC_TAB, GatewayId, -1),
    {noreply, State};
handle_cast({insert_store, Tab, Key, Value}, State) ->
    insert_store(Tab, Key, Value),
    {noreply, State};
handle_cast({delete_store, Tab, Key}, State) ->
    delete_store(Tab, Key),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {noreply, State}.

%%%===============================================================================
%% Internal functions
%%%===============================================================================

ets_init([]) ->
    ok;
ets_init([Tab | EtsList]) ->
    ets:new(Tab, [set, public, named_table, {read_concurrency, true}]),
    ets_init(EtsList).
    
insert_store(Tab, Key, Value) ->
    ets:insert(Tab, {Key, Value}).

delete_store(Tab, Key) ->
    ets:delete(Tab, Key).

lookup_for_store(Tab, Key) ->
    case ets:lookup(Tab, Key) of
        [] ->
            {error, not_found};
        [{Key, Value}] ->
            {ok, Value}
    end.


