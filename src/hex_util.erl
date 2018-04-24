-module(hex_util).

%% Eunit
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([to_hex/1]).
-export([to_bin/1]).
-export([to_int/1]).
-export([hexdigit/1]).

%%%=========================================================
%% Hex convert
%%%=========================================================

-spec(to_hex(integer | iolist()) -> string()).
%% convert an iolist to a hexadecimal string.
to_hex(0) ->
    "0";
to_hex(I) when is_integer(I), I > 0 ->
    to_hex_int(I, []);
to_hex(B) ->
    to_hex(iolist_to_binary(B), []).

to_hex(<<>>, Acc) ->
    lists:reverse(Acc);
to_hex(<<C1:4, C2:4, Rest/binary>>, Acc) ->
    to_hex(Rest, [hexdigit(C2), hexdigit(C1) | Acc]).

to_hex_int(0, Acc) ->
    Acc;
to_hex_int(I, Acc) ->
    to_hex_int(I bsr 4, [hexdigit(I band 15) | Acc]).

-spec(hexdigit(integer()) -> char()).
%% Convert an integer less than 16 to a hex digit.
hexdigit(C) when C >= 0, C =< 9 ->
    C + $0;
hexdigit(C) when C =< 15 ->
    C + $a - 10.

-spec(to_bin(string()) -> binary()).
to_bin(L) ->
    to_bin(L, []).

to_bin([], Acc) ->
    iolist_to_binary(lists:reverse(Acc));
to_bin([C1, C2 | Rest], Acc) ->
    to_bin(Rest, [(dehex(C1) bsl 4) bor dehex(C2) | Acc]).

-spec(dehex(char()) -> integer()).
%% Convert a hex digit to its integer value.
dehex(C) when C >= $0, C =< $9 ->
    C - $0;
dehex(C) when C >= $a, C =< $f ->
    C - $a + 10;
dehex(C) when C >= $A, C =< $F ->
    C - $A + 10.

-spec(to_int(string()) -> integer()).
%% Convert a hexadecimal string to an integer.
to_int(L) when is_list(L) ->
    erlang:list_to_integer(L, 16).

%% Eunit
-ifdef(EUNIT).

to_hex_test() ->
    [
     ?assertEqual("0", to_hex(0)),
     ?assertEqual("ff0000fe", to_hex([16#FF, 16#00, 16#00, 16#FE])),
     ?assertEqual("FF0000FE", string:to_upper(to_hex([16#FF, 16#00, 16#00, 16#FE]))),
     ?assertEqual("12345678", to_hex([16#12, 16#34, 16#56, 16#78]))
    ].

to_bin_test() ->
    [
     ?assertEqual(<<16#0>>, to_bin("00")),
     ?assertEqual(<<16#FF, 16#00, 16#00, 16#FE>>, to_bin("FF0000FE")),
     ?assertEqual(<<16#FF, 16#00, 16#00, 16#FE>>, to_bin("ff0000fe"))
    ].

to_int_test() ->
    [
     ?assertEqual(16#0, to_int("0")),
     ?assertEqual(16#FFFE, to_int("FFFE")),
     ?assertEqual(16#FFFE, to_int("fffe"))
    ].

hexdigit_test() ->
    [
     ?assertEqual($1, hexdigit(1)),
     ?assertEqual($2, hexdigit(2)),
     ?assertEqual($f, hexdigit(15)),
     ?assertEqual($a, hexdigit(10))
    ].

-endif.









