
-module(app_util).

%% export functions
-export([
    start_app/1, 
    stop_app/1
]).

-export([
    start_child/2,
    start_child/3
]).

%% 启动app
start_app(App) ->
    %% restart_type(重启类型): permanent(永久), transient(短暂), temporary(临时)
    %% 默认的重启类型是：temporary
    start_app_ok(App, application:start(App, permanent)).

start_app_ok(_App, ok) -> ok;
start_app_ok(_App, {error, {already_started, _App}}) -> ok;
start_app_ok(App, {error, {not_started, Dep}}) ->
    ok = start_app(Dep),
    start_app(App);
start_app_ok(App, {error, Reason}) ->
    erlang:error({app_start_failed, App, Reason}).


%% 停止app
stop_app(App) ->
    application:stop(App).


%% 创建子进程
start_child(Sup, {supervisor, Module}) ->
    supervisor:start_child(Sup, supervisor_spec(Module));
start_child(Sup, Module) ->
    {ok, _ChildId} = supervisor:start_child(Sup, worker_spec(Module)).

start_child(Sup, {supervisor, Module}, Opts) ->
    supervisor:start_child(Sup, supervisor_spec(Module, Opts));
start_child(Sup, Module, Opts) ->
    supervisor:start_child(Sup, worker_spec(Module, Opts)).

supervisor_spec(Module) when is_atom(Module) ->
    supervisor_spec(Module, start_link, []).

supervisor_spec(Module, Opts) ->
    supervisor_spec(Module, start_link, [Opts]).

supervisor_spec(M, F, A) ->
    {M, {M, F, A}, permanent, infinity, supervisor, [M]}.

worker_spec(Module) when is_atom(Module) ->
    worker_spec(Module, start_link, []).

worker_spec(Module, Opts) when is_atom(Module) ->
    worker_spec(Module, start_link, [Opts]).

worker_spec(M, F, A) ->
    {M, {M, F, A}, permanent, 10000, worker, [M]}.
