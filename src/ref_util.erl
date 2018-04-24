-module(ref_util).

-compile(export_all).

update_network(ParamStr) ->
    Dir = filename:absname(""),   
    Cmd = Dir ++ "/ref/update_network.sh " ++ ParamStr,
    seteuid(Cmd).

seteuid(ParamStr) ->
    Cmd = lists:concat(["./ref/seteuid \"", ParamStr, "\""]),
    io:format("cmd: ~s~n", [Cmd]),
    Result = os:cmd(Cmd),
    io:format("seteuid: ~s~n", [helper_util:to_iolist(Result)]),
    ok.
