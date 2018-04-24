%% Config info

%% helper util macro
-define(HELPER, helper_util).

-define(HELP, help).

-define(CALHELP, calHelp).

%% Gateway/Meter id bytes_size macro
-define(DEVICE_ID_BYTES_SIZE, 6).

%% Record separator
-define(RS, "\r\n").

%% Active report datafieldflag list

%% NB摄像水表特征数据域
-define(NB_DATAFLAG_LIST, 
        [
         <<16#34, 16#33, 16#10, 16#EF>>
        ]).
%% Data msg datafidldflag list
%% 定时上报
-define(DATAMSG_DATAFIELDFLAG_LIST,
        [
         %% 分体空调
         <<16#38, 16#32, 16#FF, 16#EF>>,
         %% 四路灯控面板 插座表 灯控计量模块
         <<16#34, 16#32, 16#FF, 16#EF>>,
         %% 环境检测仪
         <<16#32, 16#33, 16#21, 16#EF>>,
         %% 中央空调
         <<16#38, 16#32, 16#DD, 16#EF>>
        ]).

%% 控制上报
-define(CONTROL_DATAFIELDFLAG_LIST,
        [
         %% 计量灯控控制上报
         <<16#35, 16#36, 16#0F, 16#EF>>,
         %% 单纯群控，复合场景1，2开关的四个按键控制上报
         <<16#34, 16#35, 16#0F, 16#EF>>,
         <<16#35, 16#35, 16#0F, 16#EF>>,
         <<16#36, 16#35, 16#0F, 16#EF>>,
         <<16#37, 16#35, 16#0F, 16#EF>>,
         %% 策略控制上报
         <<16#34, 16#3B, 16#0F, 16#EF>>,
         <<16#35, 16#3B, 16#0F, 16#EF>>,
         <<16#36, 16#3B, 16#0F, 16#EF>>,
         <<16#37, 16#3B, 16#0F, 16#EF>>
        ]).

%% WARN msg datafieldflag list
-define(WARNMSG_DATAFIELDFLAG_LIST,
        [
         %% 功率超限报警
         <<16#35, 16#32, 16#FF, 16#EF>>
        ]).

%% Long length datafieldflag list
-define(LONG_LENGTH_DATAFIELDFLAG_LIST,
        [
         %% 设置/查询网关内存储单灯安装信息命令
         <<16#34, 16#3A, 16#0F, 16#EF>>,
         %% PWM数据查询
         <<16#63, 16#33, 16#21, 16#EF>>,
         %% 电网参数数据块查询
         <<16#64, 16#33, 16#21, 16#EF>>,
         %% 网关聚合结果上报
         <<16#39, 16#3A, 16#0F, 16#EF>>
        ]).

%% Share_lib dir
-define(SHARE_LIB_DIR, "./share_lib").

%% Listen options config filepath
-define(LISTEN_OPTS_TABLE, connector_listen_opts).
-define(LISTEN_OPTS_FILEPATH, "rel/listen.config").

%% Env options config filepath
-define(ENV_OPTS_FILEPATH, "rel/env.config").

%% Timer options config filepath
-define(TIME_OPTS_FILEPATH, "rel/time.config").

%% Computer options config filepath
-define(COMPUTER_OPTS_FILEPATH, "rel/computer.config").

%% Network config file
-define(NETWORK_CONFIG_FILE, "/etc/sysconfig/network-scripts/ifcfg-eth0").
-define(IPADDR, "IPADDR").
-define(NETMASK, "NETMASK").
-define(GATEWAY, "GATEWAY").
-define(DNS1, "DNS1").

%% 计量表数据块上报标识
-define(DATAMSG, "dataMsg").
%% 采集器上线标识
-define(STATUSMSG, "statusMsg").
%% 计量表功率超限标识
-define(WARNMSG, "warnMsg").
%% 控制上报
-define(CONTROLMSG, "controlMsg").


%% Db server client request path to fun 
-define(PATH_TO_FUN,
        [
         {<<"addTask">>, add_task},
         {<<"deleteTask">>, delete_task},
         {<<"updateTask">>, update_task},
         {<<"getTask">>, get_task},

         {<<"addDeviceTypeInfo">>, add_device_type_info},
         {<<"deleteDeviceTypeInfo">>, delete_device_type_info},
         {<<"getDeviceTypeInfo">>, get_device_type_info},
         {<<"getCollectorTypeInfo">>, get_collector_type_info},
         {<<"getRepeatersTypeInfo">>, get_repeaters_type_info},
         {<<"getMeterTypeInfo">>, get_meter_type_info},

         {<<"addCollector">>, add_collector},
         {<<"updateCollector">>, update_collector},
         {<<"deleteCollector">>, delete_collector},
         {<<"forceDeleteCollector">>, force_delete_collector},

         {<<"addMeter">>, add_meter},
         {<<"updateMeter">>, update_meter},
         {<<"deleteMeter">>, delete_meter},
         {<<"deleteMeterByCollector">>, delete_meter_by_collector},

         {<<"getCollectorInfoTotalRow">>, get_collector_info_total_row},
         {<<"getCollectorInfo">>, get_collector_info},

         {<<"getRepeatersInfoTotalRow">>, get_repeaters_info_total_row},
         {<<"getRepeatersInfo">>, get_repeaters_info},
         
         {<<"getMeterInfoTotalRow">>, get_meter_info_total_row},
         {<<"getMeterInfo">>, get_meter_info},

         {<<"getMeterNetworkByCollector">>, get_meter_network_by_collector},
         {<<"getCollectorInfoByEqptType">>, get_collector_info_by_eqpt_type},
         {<<"getRepeatersInfoByEqptType">>, get_repeaters_info_by_eqpt_type},

        
         {<<"batchAddCollector">>, batch_add_collector},
         {<<"batchAddMeter">>, batch_add_meter},
         {<<"batchGetMeter">>, batch_get_meter},

         {<<"getVersion">>, get_version},
         {<<"getStatus">>, get_status},

         {<<"getNetwork">>, get_network},
         {<<"updateNetwork">>, update_network},
         {<<"restartNetwork">>, restart_network},

         {<<"hello">>, hello}
        ]). 


