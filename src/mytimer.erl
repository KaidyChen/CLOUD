-module(mytimer).

-export([
         start_timer/3,
         start_timer/4,
         cancel_timer/1
        ]).


%%--------------------------------------------------------------------------
%% Mytimer functions
%%--------------------------------------------------------------------------

start_timer(Time, Dest, Msg) ->
    start_timer(Time, Dest, Msg, []).

start_timer(Time, Dest, Msg, Opts) ->
    erlang:start_timer(Time, Dest, Msg, Opts).

cancel_timer(TimerRef) when is_reference(TimerRef) ->
    erlang:cancel_timer(TimerRef, [{async, true}, {info, false}]);
cancel_timer(_) ->
    ok.
