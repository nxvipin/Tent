-module(tent_metainfo_SUITE).
-compile(export_all).

-include_lib("tent/include/tent.hrl").
-include_lib("tent/include/bencode.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
    [single_file,
     multi_file].

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

single_file(Config) ->
    DataDir = ?config(data_dir, Config),
    FileName = "NetflixPrizeDataSet.torrent",
    File = filename:join(DataDir, FileName),
    {ok, #metainfo{announce=Announce,
                   info_hash=InfoHash,
                   info=#info{piece_length=PieceLength,
                              data=#single_file{}}=Data}}
        = tent_metainfo:get(File),

    ?assertEqual(Announce, <<"http://academictorrents.com/announce.php"
                             "?passkey=47b4233fa715edcdc7024bf5cba0b889">>),
    ?assertEqual(tent_utils:urlencode(<<"\x9B\x13\x18\x3D\xC4"
                                        "\xD6\x06\x76\xB7\x73"
                                        "\xC9\xE2\xCD\x6D\xE5"
                                        "\xE5\x54\x2C\xEE\x9A">>, []),
                 InfoHash),
    ?assertEqual(512*1024, PieceLength),
    ok.

multi_file(Config) ->
    DataDir = ?config(data_dir, Config),
    FileName = "VincentVanGoghPaintings.torrent",
    File = filename:join(DataDir, FileName),
    {ok, #metainfo{announce=Announce,
                   info_hash=InfoHash,
                   info=#info{piece_length=PieceLength,
                              data=#multi_file{files=Files}}}=Data}
        = tent_metainfo:get(File),

    ?assertEqual(Announce, <<"http://academictorrents.com/announce.php"
                             "?passkey=4f5da11b96ede368e1e62341a15fa9f2">>),
    ?assertEqual(tent_utils:urlencode(<<"\xC8\xB6\x87\xC9\x84"
                                        "\xD3\xD9\x02\x31\x0F"
                                        "\x27\xD5\x67\x59\xED"
                                        "\x69\xF5\xE1\xB4\xA7">>, []),
                 InfoHash),
    ?assertEqual(256*1024, PieceLength),
    ?assertEqual(2036, length(Files)),

    ok.
