-module(connector_db_store_server).

-behaivor(gen_server).

-include("print.hrl").

-export([start_link/0]).

-export([
         get_gateway_type/1,
         notify_update_task_info/0,
         notify_update_eqpt_info/0
        ]).

-export([
         get_gateway_type/1,
         get_eqpt_type/2,
         get_upper_eqpt_type_and_upper_eqpt_id_code/2,
         get_collector2/2,
         get_dll_prefix/2,
         get_protocol_type/2,
         get_protocol_type_by_gateway_and_meter/2,
         get_protocol_type_by_eqpt_type/1,
         get_eqpt_level_and_protocol_type_by_eqpt_type/1
        ]).

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-define(SERVER, ?MODULE).

-define(EMPTY_STR, "").
-define(DLL_SEPARATOR, "_").

-record(eqpt_info_rd, {
          key,
          eqptIdCode, 
          eqptType, 
          eqptCjqType, 
          eqptCjqCode, 
          eqptZjqType,
          eqptZjqCode,
          eqptProtocolType,
          eqptCjqProtocolType
         }).

-record(eqpt_id_rd, {
          key,
          eqptIdCode 
         }).

-record(eqpt_task_rd, {
          key,
          taskId,  
          eqptCjqType, 
          eqptCjqIdCode, 
          meterType,
          meterIdCode,
          taskName,
          taskCmdId,
          taskStart,
          taskTime
         }).

-record(eqpt_type_rd, {
          eqptType,
          protocolType
         }).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

notify_update_eqpt_info() ->
    gen_server:cast(?SERVER, notify_update_eqpt_info).
 
notify_update_task_info() ->
    gen_server:cast(?SERVER, notify_update_task_info).

init([]) ->
    State = #state{
              },
    {ok, State, 0}.

handle_call(_Request, _From = {_Pid, _Tag}, State) -> 
    {noreply, State}.

handle_cast(notify_update_eqpt_info, State) ->
    store_eqpt_info(),
    {noreply, State};

handle_cast(notify_update_task_info, State) ->
    store_eqpt_task(),
    {noreply, State};
    
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    connector_eqpt_info_store:init(#eqpt_info_rd.key),
    store_eqpt_info(),
    connector_eqpt_type_info_store:init(),
    store_eqpt_type_info(),
    connector_eqpt_task_store:init(#eqpt_task_rd.key),
    store_eqpt_task(),
    connector_eqpt_id_store:init(#eqpt_id_rd.key),
    store_eqpt_id(),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

store_eqpt_info() ->
    {ok, Ref} = db_util:load_eqpt_info(),
    read_row_info(Ref).

store_eqpt_task() ->
    {ok, Ref} = db_util:load_eqpt_task(),
    read_row_task(Ref).

store_eqpt_id() ->
    {ok, Ref} = db_util:load_eqpt_id(),
    read_row_id(Ref).

read_row_info(Ref) ->
    case db_util:read_row(Ref) of
        done ->
            db_util:finalize(Ref);
        Row ->
            EqptInfoRd = gen_eqpt_info_rd(Row),
            connector_eqpt_info_store:insert(EqptInfoRd),
            read_row_info(Ref)
    end.
read_row_task(Ref) ->
    case db_util:read_row(Ref) of
        done ->
            db_util:finalize(Ref);
        Row ->
            EqptTaskRd = gen_eqpt_task_rd(Row),
            connector_eqpt_task_store:insert(EqptTaskRd),
            read_row_task(Ref)
    end.

read_row_id(Ref) ->
    case db_util:read_row(Ref) of
        done ->
            db_util:finalize(Ref);
        Row ->
            EqptIdRd = gen_eqpt_id_rd(Row),
            connector_eqpt_id_store:insert(EqptIdRd),
            read_row_id(Ref)
    end.

gen_eqpt_info_rd(Row) ->
    {EqptIdCodeBinary, EqptTypeBinary, EqptCjqTypeBinary, EqptCjqCodeBinary, EqptZjqTypeBinary, 
     EqptZjqCodeBinary, EqptProtocolTypeBinary, EqptCjqProtocolTypeBinary} = format_row(Row),
    EqptType = binary_to_list(EqptTypeBinary), 
    EqptIdCode = binary_to_list(EqptIdCodeBinary),
    #eqpt_info_rd{
       key = {EqptType, EqptIdCode},
       eqptIdCode = EqptIdCode,
       eqptType = EqptType,
       eqptCjqType = binary_to_list(EqptCjqTypeBinary), 
       eqptCjqCode = binary_to_list(EqptCjqCodeBinary), 
       eqptZjqType = binary_to_list(EqptZjqTypeBinary),
       eqptZjqCode = binary_to_list(EqptZjqCodeBinary),
       eqptProtocolType = binary_to_list(EqptProtocolTypeBinary),
       eqptCjqProtocolType = binary_to_list(EqptCjqProtocolTypeBinary)
      }.

gen_eqpt_id_rd(Row) ->
    {EqptCjqTypeBinary, EqptCjqCodeBinary, EqptTypeBinary, EqptIdCodeBinary} = format_row(Row),
    EqptCjqType = binary_to_list(EqptCjqTypeBinary), 
    EqptCjqCode = binary_to_list(EqptCjqCodeBinary),
    EqptType = binary_to_list(EqptTypeBinary),
    #eqpt_id_rd{
      key = {EqptCjqType, EqptCjqCode, EqptType},
      eqptIdCode = binary_to_list(EqptIdCodeBinary)
      }.
  
gen_eqpt_task_rd(Row) ->
    {TaskIdBinary, EqptCjqTypeBinary, EqptCjqIdCodeBinary, MeterTypeBinary, MeterIdCodeBinary,  
     TaskNameBinary, TaskCmdIdBinary, TaskTypeBinary, TaskStartBinary, TaskTimeBinary} = format_row(Row),
    TaskType = binary_to_list(TaskTypeBinary),
    #eqpt_task_rd{
       key = TaskType,
       taskId = binary_to_list(TaskIdBinary),
       eqptCjqType = binary_to_list(EqptCjqTypeBinary), 
       eqptCjqIdCode = binary_to_list(EqptCjqIdCodeBinary), 
       meterType = binary_to_list(MeterTypeBinary),
       meterIdCode = binary_to_list(MeterIdCodeBinary),
       taskName = binary_to_list(TaskNameBinary),
       taskCmdId = binary_to_list(TaskCmdIdBinary),
       taskStart = binary_to_list(TaskStartBinary),
       taskTime = binary_to_list(TaskTimeBinary)
      }.

store_eqpt_type_info() ->
    case db_util:select_eqpt_type_and_protocol_type() of
        {ok, Rows} ->
            EqptTypeAndProtocolTypeList = [gen_eqpt_type_info(Row) || 
                Row <- Rows],
            connector_eqpt_type_info_store:insert(EqptTypeAndProtocolTypeList);
        {error, Reason} ->
            ?ERROR("store_eqpt_type_info is error:~p~n", [Reason]),
            {error, Reason}
    end.

gen_eqpt_type_info(Row) ->
    {EqptTypeBinary, EqptLevelInteger, ProtocolTypeBinary} = format_row(Row),
    {binary_to_list(EqptTypeBinary), {EqptLevelInteger, binary_to_list(ProtocolTypeBinary)}}.

format_row(Row) ->
    Fun = 
        fun(Item) ->
                format_null(Item)
        end,
    list_to_tuple(lists:map(Fun, tuple_to_list(Row))).
    
format_null(null) ->
    list_to_binary(?EMPTY_STR);
format_null(Item) ->
    Item.

get_gateway_type(GatewayId) ->
    EqptInfoRdTmp = #eqpt_info_rd{
                       key = '_',
                       eqptIdCode = GatewayId,
                       eqptType = '_',
                       eqptCjqType = '_',
                       eqptCjqCode = '_',
                       eqptZjqType = '_',
                       eqptZjqCode = '_',
                       eqptProtocolType = '_',
                       eqptCjqProtocolType = '_'
                   },
    case  connector_eqpt_info_store:match_object(EqptInfoRdTmp) of
        {ok, EqptInfoRd} ->
            {ok, EqptInfoRd#eqpt_info_rd.eqptType};
        {error, Reason} ->
            {error, Reason}
    end.
           
get_eqpt_type(GatewayId, EqptIdCode) ->
    EqptInfoRdTmp = #eqpt_info_rd{
                       key = '_',
                       eqptIdCode = EqptIdCode,
                       eqptType = '_',
                       eqptCjqType = '_',
                       eqptCjqCode = GatewayId,
                       eqptZjqType = '_',
                       eqptZjqCode = '_',
                       eqptProtocolType = '_',
                       eqptCjqProtocolType = '_'
                   },
    case  connector_eqpt_info_store:match_object(EqptInfoRdTmp) of
        {ok, EqptInfoRd} ->
            {ok, EqptInfoRd#eqpt_info_rd.eqptType};
        {error, Reason} ->
            {error, Reason}
    end.

get_collector2(GatewayId, EqptIdCode) ->
    EqptInfoRdTmp = #eqpt_info_rd{
                       key = '_',
                       eqptIdCode = EqptIdCode,
                       eqptType = '_',
                       eqptCjqType = '_',
                       eqptCjqCode = GatewayId,
                       eqptZjqType = '_',
                       eqptZjqCode = '_',
                       eqptProtocolType = '_',
                       eqptCjqProtocolType = '_'
                      },
    case  connector_eqpt_info_store:match_object(EqptInfoRdTmp) of
        {ok, EqptInfoRd} ->
            Collector2 = EqptInfoRd#eqpt_info_rd.eqptZjqCode,
            {ok, Collector2};
        {error, Reason} ->
            {ok, ""}
    end.

get_upper_eqpt_type_and_upper_eqpt_id_code(EqptType, EqptIdCode) ->
    case connector_eqpt_info_store:lookup({EqptType, EqptIdCode}) of
        {ok, EqptInfoRd} ->
            #eqpt_info_rd{
               eqptCjqType = EqptCjqType,
               eqptCjqCode = EqptCjqCode
              } = EqptInfoRd,
            {ok, {EqptCjqType, EqptCjqCode}};
        {error, Reason} ->
            {error, Reason}
    end.

get_dll_prefix(EqptType, EqptIdCode) ->
    case connector_eqpt_info_store:lookup({EqptType, EqptIdCode}) of
        {ok, EqptInfoRd} ->
            #eqpt_info_rd{
               eqptProtocolType = EqptProtocolType,
               eqptCjqProtocolType = EqptCjqProtocolType
              } = EqptInfoRd,
            case EqptProtocolType of
                "" ->
                    {ok, EqptCjqProtocolType};
                _ ->
                    {ok, string:join([EqptProtocolType, EqptCjqProtocolType], ?DLL_SEPARATOR)}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

get_protocol_type(EqptType, EqptIdCode) ->
    case connector_eqpt_info_store:lookup({EqptType, EqptIdCode}) of
        {ok, EqptInfoRd} ->
            #eqpt_info_rd{
               eqptProtocolType = EqptProtocolType
              } = EqptInfoRd,
            {ok, EqptProtocolType};
        {error, Reason} ->
            {error, Reason}
    end.

get_protocol_type_by_gateway_and_meter(GatewayId, MeterId) ->
    EqptInfoRdTmp = #eqpt_info_rd{
                       key = '_',
                       eqptIdCode = MeterId,
                       eqptType = '_',
                       eqptCjqType = '_',
                       eqptCjqCode = GatewayId,
                       eqptZjqType = '_',
                       eqptZjqCode = '_',
                       eqptProtocolType = '_',
                       eqptCjqProtocolType = '_'
                      },
    case  connector_eqpt_info_store:match_object(EqptInfoRdTmp) of
        {ok, EqptInfoRd} ->
            #eqpt_info_rd{
               eqptProtocolType = EqptProtocolType
              } = EqptInfoRd,
            {ok, EqptProtocolType};
        {error, Reason} ->
            {error, Reason}
    end.
    

get_protocol_type_by_eqpt_type(EqptType) ->
    case connector_eqpt_type_info_store:lookup(EqptType) of
        {ok, {_EqptLevel, ProtocolType}} ->
            {ok, ProtocolType};
        {error, Reason} ->
            {error, Reason}
    end.

get_eqpt_level_and_protocol_type_by_eqpt_type(EqptType) ->
    case connector_eqpt_type_info_store:lookup(EqptType) of
        {ok, {EqptLevel, ProtocolType}} ->
            {ok, EqptLevel, ProtocolType};
        {error, Reason} ->
            {error, Reason}
    end.
    
