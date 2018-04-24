-record(cmd_obj, {
        eqpt_type :: string(),      % 设备类型
        eqpt_id_code :: string(),   % 设备号
        cmd_type :: string(),       % 命令类型
        cmd_id :: string(),         % 命令id
        cmd_data :: string()       % 命令的转入参数
}).
-type cmd_obj() :: #cmd_obj{}.