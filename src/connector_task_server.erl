-module(connector_task_server).

-behaivor(gen_server).

-include("config.hrl").
-include("print.hrl").

-export([start/0]).
-export([run_normal_task/1, run_polling_task/1]).

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-define(SERVER, ?MODULE).

-define(NormalTask, normal_tab).
-define(PollingTask, polling_tab).

-define(TIMEOUT, 60*1000).

-record(state, {}).

start() ->
    [{task_process_opts, TaskOpts}]= ets:lookup(?LISTEN_OPTS_TABLE, task_process_opts),
    case proplists:get_value(taskswitch, TaskOpts) of
        "on" ->
            start_link();
        "off" ->
            io:format("task close!!!~n",[]),
            ok
    end.

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
    
init([]) ->
    connector_meter_task_store:init(?NormalTask),
    connector_meter_task_store:init(?PollingTask),
    connector_task_pid_store:init(),
    State = #state{},
    {ok, State, 10000}.

handle_call(_Request, _From, State) -> 
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    check_eqpt_task(),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%------------------------------------------------------------------------------------------------
%TaskList里可能只含有一个任务也可能含有多个任务，有些任务中表计id可能为空，需要联合ets表查找出表计
% id，然后依次组合成单一任务列表存进ets表中
% 1、先从ets表中查询获得任务列表
% 2、判断任务例表是否为空
% 3、从列表中依次获取单个任务，判断该任务中的表计id是否为空，若为空联合查询获取所有该类型的表计id，
%    再依次重组成单个任务插入到ets任务列表中，若不为空正常进行
% 4、创建任务处理进程，从处理后的的任务表中导出任务，将任务作为参数依次传入
%%------------------------------------------------------------------------------------------------
check_eqpt_task() ->
    {ok, TaskList1} = connector_eqpt_task_store:lookup("xs"),
    {ok, TaskList2} = connector_eqpt_task_store:lookup("rd"), 
    {ok, TaskList3} = connector_eqpt_task_store:lookup("yd"),
    {ok, TaskList4} = connector_eqpt_task_store:lookup("lx"),
    get_task_list(?NormalTask, TaskList1),
    get_task_list(?NormalTask, TaskList2),
    get_task_list(?NormalTask, TaskList3),
    get_task_list(?PollingTask, TaskList4),
    NormalTaskList = connector_meter_task_store:show(?NormalTask),
    PollingTaskList =  connector_meter_task_store:show(?PollingTask),
    ?PRINT("NormalTaskList:~p~n",[NormalTaskList]),
    ?PRINT("PollingTaskList:~p~n",[PollingTaskList]),
    case NormalTaskList =:= [] of
          true ->
              ok;
          false ->
              lists:foreach(fun(Task) -> run_normal_task(Task) end, NormalTaskList)
    end,
    case PollingTaskList =:= [] of
          true ->
              ok;
          false ->
              lists:foreach(fun(Task) -> run_polling_task(Task) end, PollingTaskList)
    end.

get_task_list(EtsName, TaskList) ->
    case TaskList of
          [] ->
              ?PRINT("tasklist is null~n",[]),
              ok;
          _ ->
              Fun = 
                  fun(TaskTmp) ->
                          {_, TaskType, TaskId, EqptCjqType, EqptCjqIdCode, MeterType, MeterIdCode, TaskName, TaskCmdId, TaskStart, TaskTime} = TaskTmp,
                          case MeterIdCode =:= [] of
                              false ->
                                    Task = {TaskId, MeterType, MeterIdCode, TaskCmdId,TaskType, TaskStart,TaskTime},
                                    connector_meter_task_store:insert(EtsName, Task);
                              true ->
                                    {ok, MeterIdCodeList} = connector_eqpt_id_store:lookup({EqptCjqType, EqptCjqIdCode, MeterType}),
                                    [connector_meter_task_store:insert(EtsName, {TaskId, MeterType, NewMeterIdCode, TaskCmdId,TaskType, TaskStart,TaskTime}) || {_, _, NewMeterIdCode} <- MeterIdCodeList]  
                          end
                  end,
              lists:foreach(Fun, TaskList)
    end.

run_normal_task(Task) ->
    %%Pid = spawn(fun() -> connector_task_process:start_link(Task) end),
    {ok, Pid} = connector_task_process:start_link(Task),
    TaskList = tuple_to_list(Task),
    TaskId = lists:nth(1, TaskList),
    TaskType = lists:nth(5, TaskList),
    connector_task_pid_store:insert(TaskId, TaskType, Pid),
    ?PRINT("Pid:~p Task:~p~n",[Pid,Task]).

run_polling_task(Task) ->
    %%Pid = spawn(fun() -> connector_pollingtask_process:start_link(Task) end),
    {ok, Pid} = connector_pollingtask_process:start_link(Task),
    TaskList = tuple_to_list(Task),
    TaskId = lists:nth(1, TaskList),
    TaskType = lists:nth(5, TaskList),
    connector_task_pid_store:insert(TaskId, TaskType, Pid),
    ?PRINT("Pid:~p Task:~p~n",[Pid,Task]).

