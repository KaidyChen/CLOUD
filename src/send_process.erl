-module(send_process).
-export([send/1]).
-export([get_result/1]).

-include("cmd_obj.hrl").
-include("print.hrl").
-include("config.hrl").

-define(SUCCESS_STATUS, <<"200">>).

send(Cmd_obj) ->
    send(Cmd_obj, 100).

send(Cmd_obj, Cq_weight) ->
    if
        Cq_weight =< 20 -> 
            ?PRINT_MSG("cq =< 20"), 
            "cq =< 20";
        true -> 
            [{task_process_opts, TaskProcessOpts}] = ets:lookup(?LISTEN_OPTS_TABLE, task_process_opts),
            Timeout = proplists:get_value(timeout, TaskProcessOpts),
            Url = proplists:get_value(url, TaskProcessOpts),
            OldPriority = proplists:get_value(task_priority, TaskProcessOpts, 0),
            TaskPriority = gen_priority(Cmd_obj, OldPriority), 
            Cmd_json = cmd_obj_to_json(Cmd_obj, TaskPriority), 
            send_cmd(Url, Cmd_json, Timeout)
    end.

gen_priority(#cmd_obj{cmd_type = "dsj"}, _OldPriority) ->
    random:uniform(10) + 10;
gen_priority(_, OldPriority) ->
    OldPriority.
    
cmd_obj_to_json(Cmd_obj, TaskPriority)->
    Order_no = get_order_no:order_no(),
    Partner = "6ccpug",
    Eqpt_pwd = "0DD1F70B5F3A0B95C22126872845114A",
    Eqpt_type = Cmd_obj#cmd_obj.eqpt_type,
    Eqpt_id_code = Cmd_obj#cmd_obj.eqpt_id_code,
    Cmd_type = Cmd_obj#cmd_obj.cmd_type,
    Cmd_id = Cmd_obj#cmd_obj.cmd_id,
    Cmd_data = Cmd_obj#cmd_obj.cmd_data,
    Cmd_json = lists:concat(["order_no=", Order_no, "&partner=", Partner,"&objs=[{",
    "\"eqpt_type\"", ":","\"", Eqpt_type, "\"", "," , "\"eqpt_id_code\"", ":", "\"", Eqpt_id_code, "\"", ",", 
    "\"eqpt_pwd\"" , ":", "\"", Eqpt_pwd, "\"", "," , "\"resdatatype\"" , ":" ,"\"" , "\"" ,"," ,
    "\"cmd_type\"" , ":" , "\"" , Cmd_type,"\"" , "," ,"\"cmd_id\"" , ":" , "\"",Cmd_id , "\"" , ",",
    "\"cmd_data\"" , ":" , "\"", Cmd_data , "\"","}]" , "&sign=FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF&priority=", 
                             integer_to_list(TaskPriority)]),
    io:format("cmd_obj:~p~n",[Cmd_json]),
    Cmd_json.

send_cmd(Url, ReqBody, Timeout) ->
    httpc:request(post, {Url, [], "application/x-www-form-urlencoded", ReqBody}, [{timeout, Timeout}, {connect_timeout, Timeout}], []).
                
get_result(Return_data) ->
    try
        case string:tokens(Return_data, "&") of
            [JsonStr | _] ->
                io:format("JsonStr:~s~n", [JsonStr]),
                FieldMap = jsx:decode(helper_util:to_iolist(JsonStr), [return_maps]),
                case maps:find(<<"result">>, FieldMap) of
                    {ok, ResultBits} ->
                        case maps:find(<<"status">>, FieldMap) of
                            {ok, ?SUCCESS_STATUS} ->
                                {ok, binary_to_list(ResultBits)};
                            {ok, _} ->
                                {false, binary_to_list(ResultBits)};
                            _ ->
                                false
                        end;
                    _ ->
                        false
                end;
            _ ->
                false
        end
    catch
        _:_ ->
            false
    end.
