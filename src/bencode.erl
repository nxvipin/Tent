-module(bencode).
-include("bencode.hrl").

-export([decode/1, encode/1]).

-spec decode(binary()) -> {ok, bval()} | {error, any()}.
decode(Data) ->
    try
        {Result, _} = decode_(Data),
        {ok, Result}
    catch _:Error ->
            {error, Error}
    end.

-spec decode_(binary()) -> {bval(), binary()}.
decode_(<<$l, Data/binary>>) ->
    decode_list(Data, []);
decode_(<<$d, Data/binary>>) ->
    decode_dict(Data, maps:new());
decode_(<<$i, Data/binary>>) ->
    decode_integer(Data);
decode_(Data) ->
    decode_string(Data).

-spec decode_dict(binary(), map()) -> {bdict(), binary()}.
decode_dict(<<$e, Data/binary>>, Acc) ->
    {Acc, Data};
decode_dict(Data, Acc) ->
    {Key, Tail0} = decode_string(Data),
    {Value, Tail} = decode_(Tail0),
    decode_dict(Tail, maps:put(Key, Value, Acc)).

-spec decode_list(binary(), [bval()]) -> {[bval()], binary()}.
decode_list(<<$e, Data/binary>>, Acc) ->
    {lists:reverse(Acc), Data};
decode_list(Data, Acc) ->
    {Value, Tail} = decode_(Data),
    decode_list(Tail, [Value | Acc]).

-spec decode_integer(binary()) -> {bint(), binary()}.
decode_integer(Data) ->
    [Value, Tail] = binary:split(Data, <<$e>>),
    case Value of
        <<$0, _>> -> erlang:error(badarg);
        <<$-, $0>> -> erlang:error(badarg);
        <<$-, $0, _>> -> erlang:error(badarg);
        _ -> ok
    end,
    {binary_to_integer(Value), Tail}.

-spec decode_string(binary()) -> {bstr(), binary()}.
decode_string(Data) ->
    [StrLength0, Tail0] = binary:split(Data, <<":">>),
    StrLength = binary_to_integer(StrLength0),
    case Tail0 of
        <<Str:StrLength/binary, Tail/binary>> ->
            {Str, Tail};
        _ -> {Tail0, <<>>}
    end.

-spec encode(bval()) -> {ok, binary()} | {error, any()}.
encode(Data) ->
    try
        Result = erlang:iolist_to_binary(lists:reverse(encode_(Data, []))),
        {ok, Result}
    catch _:Error ->
            {error, Error}
    end.

-spec encode_(bval(), list(binary())) -> list(binary()).
encode_(Data, Acc) when is_list(Data) ->
    encode_list(Data, [<<"l">> | Acc]);
encode_(Data, Acc) when is_map(Data) ->
    encode_map(maps:to_list(Data), [<<"d">> | Acc]);
encode_(Data, Acc) when is_binary(Data) ->
    encode_string(Data, Acc);
encode_(Data, Acc) when is_integer(Data) ->
    encode_integer(Data, Acc).

-spec encode_list(list(bval()), list(binary())) -> list(binary()).
encode_list([], Acc) ->
    [<<"e">> | Acc];
encode_list([H|T], Acc) ->
    {ok, Data} = encode(H),
    encode_list(T, [Data| Acc]).

-spec encode_map(list({string(), bval()}), list(binary())) -> list(binary()).
encode_map([], Acc) ->
    [<<"e">> | Acc];
encode_map([{Key0, Val0}|T], Acc) ->
    {ok, Key} = encode(Key0),
    {ok, Val} = encode(Val0),
    encode_map(T, [Val | [Key | Acc]]).

-spec encode_string(bstr(), list(binary())) -> list(binary()).
encode_string(Data, Acc) ->
    Len = integer_to_list(byte_size(Data)),
    [erlang:iolist_to_binary([Len, <<":">>, Data]) | Acc].

-spec encode_integer(bint(), list(binary())) -> list(binary()).
encode_integer(Data, Acc) ->
    [erlang:iolist_to_binary([<<"i">>, integer_to_binary(Data), <<"e">>])
     | Acc].
