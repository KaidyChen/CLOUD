-module(connector_eqpt_type_info_store).

-export([
         init/0,
         insert/1,
         lookup/1
        ]).

-define(TAB_ID, ?MODULE).

init() ->
    ?TAB_ID = ets:new(?TAB_ID, [set, protected, named_table, {read_concurrency, true}]).

insert(ObjectOrObjects) ->
    ets:insert(?TAB_ID, ObjectOrObjects).

lookup(Key) ->
    case ets:lookup(?TAB_ID, Key) of
        [] ->
            {error, not_found};
        [{Key, Value}] ->
            {ok, Value}
    end.
