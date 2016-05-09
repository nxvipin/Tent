-module(tent_utils).
-export([urlencode/2]).

%% The `urlencode` function is a copy of the one in Hackney with minro changes.
%% Avoiding Hackney as project dependency for now.

-spec urlencode(binary() | string(), [noplus|upper]) -> binary().
urlencode(Bin, Opts) ->
    Plus = not proplists:get_value(noplus, Opts, false),
    Upper = proplists:get_value(upper, Opts, false),
    urlencode(Bin, <<>>, Plus, Upper).

-spec urlencode(binary(), binary(), boolean(), boolean()) -> binary().
urlencode(<<C, Rest/binary>>, Acc, Plus, Upper)
  when C >= $0, C =< $9 orelse C >= $A, C =< $Z orelse C >= $a, C =< $z ->
    urlencode(Rest, <<Acc/binary, C>>, Plus, Upper);
urlencode(<<C, Rest/binary>>, Acc, Plus, Upper)
  when C =:= $.; C =:= $-; C =:= $~; C =:= $_; C =:= $*; C =:= $@ ->
    urlencode(Rest, <<Acc/binary, C>>, Plus, Upper);
urlencode(<<C, Rest/binary>>, Acc, Plus, Upper)
  when C =:= $(; C =:= $); C =:= $!, C =:= $$ ->
    urlencode(Rest, <<Acc/binary, C>>, Plus, Upper);
urlencode(<<C, Rest/binary>>, Acc, Plus, Upper)
  when C =:= $ , Plus ->
    urlencode(Rest, <<Acc/binary, $+>>, Plus, Upper);
urlencode(<<C, Rest/binary>>, Acc, Plus, Upper) ->
    H = C band 16#F0 bsr 4, L = C band 16#0F,
    H1 = case Upper of true -> tohexu(H); false -> tohexl(H) end,
    L1 = case Upper of true -> tohexu(L); false -> tohexl(L) end,
    urlencode(Rest, <<Acc/binary, $%, H1, L1>>, Plus, Upper);
urlencode(<<>>, Acc, _Plus, _Upper) ->
    Acc.

-spec tohexu(byte()) -> byte().
tohexu(C) when C < 10 -> $0 + C;
tohexu(C) when C < 16 -> $A + C - 10.

-spec tohexl(byte()) -> byte().
tohexl(C) when C < 10 -> $0 + C;
tohexl(C) when C < 16 -> $a + C - 10.
