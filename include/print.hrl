%% DEBUG
-ifdef(NODEBUG).
-define(PRINT(_Format, _Args), ok).
-define(PRINT_MSG(_Msg), ok).
-else.
-define(PRINT(Format, Args), io:format(Format, Args)).
-define(PRINT_MSG(Msg), io:format(Msg)).
-endif.

-define(INFO(Format, Args), lager:info(Format, Args)).
-define(ERROR(Format, Args), lager:error(Format, Args)).

-define(INFO_MSG(Msg), lager:info(Msg)).
-define(ERROR_MSG(Msg), lager:error(Msg)).
