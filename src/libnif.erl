-module(libnif).

-export([exec_func/4]).

-on_load(init/0).

init() ->
    NifName = ?MODULE_STRING,
    {ok, Application} = application:get_application(?MODULE),
    PrivDir = 
        case code:priv_dir(atom_to_list(Application)) of
            {error, bad_name} ->
                "priv";
            Dir ->
                Dir
        end,
    NifFileName = filename:join(PrivDir, NifName),
    erlang:load_nif(NifFileName, 0).
            
exec_func(_LibName, _FuncName, _Input, _Output) ->
    error:nif_error(nif_library_not_loaded).
