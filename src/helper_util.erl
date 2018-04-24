-module(helper_util).

-compile([export_all]).

%% Convert to binary
to_binary(List) when is_list(List) ->
    list_to_binary(List);
to_binary(Binary) when is_binary(Binary) ->
    Binary;
to_binary(Integer) when is_integer(Integer) ->
    integer_to_binary(Integer);
to_binary(Float) when is_float(Float) ->
    float_to_binary(Float);
to_binary(Atom) when is_atom(Atom) ->
    to_binary(Atom, utf8);
to_binary(_) ->
    erlang:error(badarg).

to_binary(Atom, Encoding) when is_atom(Atom),(
                               (Encoding =:= latin1) orelse
                               (Encoding =:= unicode) orelse
                               (Encoding =:= utf8)) ->
    atom_to_binary(Atom, Encoding);
to_binary(_, _) ->
    erlang:error(badarg).

to_string(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
to_string(Integer) when is_integer(Integer) ->
    integer_to_list(Integer);
to_string(Float) when is_float(Float) ->
    float_to_list(Float);
to_string(String) when is_list(String) ->
    String;
to_string(Binary) when is_binary(Binary) ->
    binary_to_list(Binary).

to_iolist(String) when is_list(String)->
    unicode:characters_to_binary(String);
to_iolist(Binary) when is_binary(Binary) ->
    Binary.

to_atom(String) when is_list(String) ->
    try erlang:list_to_existing_atom(String) of
        Atom ->
            Atom
    catch
        _Class:badarg ->
            erlang:list_to_atom(String)
    end.

datetime() ->
    calendar:local_time().

datetime_string() ->
    datetime_string(datetime()).

datetime_string({{Y, M, D} ,{H, MM, S}}) ->
    lists:flatten(
      io_lib:format(
        "~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w", [Y, M, D, H, MM, S])).

get_YM_filedir({Y, M, _D}) ->
    lists:flatten(
      io_lib:format("~4..0w/~2..0w", [Y, M])).

get_YMD_filename({Y, M, D}) ->
    lists:flatten(
      io_lib:format("~4..0w-~2..0w-~2..0w", [Y, M, D])).
