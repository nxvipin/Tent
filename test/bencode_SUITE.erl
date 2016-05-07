-module(bencode_SUITE).
-compile(export_all).

-include("../include/bencode.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").


all() ->
    [decode_int,
     decode_string,
     decode_list,
     decode_dict,
     encode_int,
     encode_string,
     encode_list,
     encode_dict].

groups() ->
    [].

suite() ->
    [{timetrap, {seconds, 30}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

decode_string(_Config) ->
    ?assertEqual({ok, <<>>}, bencode:decode(<<"0:">>)),
    ?assertEqual({ok, <<"abc">>}, bencode:decode(<<"3:abc">>)).

decode_int(_Config) ->
    ?assertEqual({ok, 0}, bencode:decode(<<"i0e">>)),
    ?assertEqual({ok, -1}, bencode:decode(<<"i-1e">>)),
    ?assertEqual({error, badarg}, bencode:decode(<<"i00e">>)),
    ?assertEqual({error, badarg}, bencode:decode(<<"i-0e">>)),
    ?assertEqual({error, badarg}, bencode:decode(<<"i-01e">>)).

decode_list(_Config) ->
    ?assertEqual({ok, []}, bencode:decode(<<"le">>)),
    ?assertEqual({ok, [<<>>]}, bencode:decode(<<"l0:e">>)),
    ?assertEqual({ok, [0]}, bencode:decode(<<"li0ee">>)),
    ?assertEqual({ok, [[]]}, bencode:decode(<<"llee">>)),
    ?assertEqual({ok, [[[]], [<<>>]]}, bencode:decode(<<"llleel0:ee">>)),
    ?assertEqual({ok, [[]]}, bencode:decode(<<"llee">>)),
    ?assertEqual({ok, [[]]}, bencode:decode(<<"llee">>)).

decode_dict(_Config) ->
    ?assertEqual({ok, #{}}, bencode:decode(<<"de">>)),
    ?assertEqual({ok, #{<<"a">> => <<"b">>}}, bencode:decode(<<"d1:a1:be">>)),
    ?assertEqual({ok, #{<<"a">> => [#{<<"x">> => <<>>}, <<"yz">>, 256]}},
                 bencode:decode(<<"d1:ald1:x0:e2:yzi256eee">>)).

encode_string(_Config) ->
    ?assertEqual({ok, <<"0:">>}, bencode:encode(<<>>)),
    ?assertEqual({ok, <<"3:abc">>}, bencode:encode(<<"abc">>)).

encode_int(_Config) ->
    ?assertEqual({ok, <<"i0e">>}, bencode:encode(0)),
    ?assertEqual({ok, <<"i-1e">>}, bencode:encode(-1)).

encode_list(_Config) ->
    ?assertEqual({ok, <<"le">>}, bencode:encode([])),
    ?assertEqual({ok, <<"l0:e">>}, bencode:encode([<<>>])),
    ?assertEqual({ok, <<"li0ee">>}, bencode:encode([0])),
    ?assertEqual({ok, <<"llee">>}, bencode:encode([[]])),
    ?assertEqual({ok, <<"llleel0:ee">>}, bencode:encode([[[]], [<<>>]])),
    ?assertEqual({ok, <<"llee">>}, bencode:encode([[]])).

encode_dict(_Config) ->
    ?assertEqual({ok, <<"de">>}, bencode:encode(#{})),
    ?assertEqual({ok, <<"d1:a1:be">>}, bencode:encode(#{<<"a">> => <<"b">>})),
    ?assertEqual({ok, <<"d1:ald1:x0:e2:yzi256eee">>},
                 bencode:encode(#{<<"a">> =>
                                      [#{<<"x">> => <<>>}, <<"yz">>, 256]})).
