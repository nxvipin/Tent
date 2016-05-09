-module(tent_metainfo).
-include("tent.hrl").
-include("bencode.hrl").

-export([get/1]).

-spec get(torrent_file()) -> {ok, metainfo()}.
get(TorrentFile) ->
    %% [TODO] Handle error cases, fail gracefully
    {ok, BencodedData} = file:read_file(TorrentFile),
    {ok, Data} = bencode:decode(BencodedData),
    Announce = maps:get(<<"announce">>, Data),
    AnnounceList = maps:get(<<"announce-list">>, Data, undefined),
    CreationDate = maps:get(<<"creation date">>, Data, undefined),
    Comment = maps:get(<<"comment">>, Data, undefined),
    CreatedBy = maps:get(<<"created by">>, Data, undefined),
    Encoding = maps:get(<<"encoding">>, Data, undefined),
    Info = get_info(maps:get(<<"info">>, Data)),
    {ok, #metainfo{announce=Announce,
                   announce_list=AnnounceList,
                   creation_date=CreationDate,
                   comment=Comment,
                   created_by=CreatedBy,
                   encoding=Encoding,
                   info=Info}}.

-spec get_info(bdict()) -> info().
get_info(Data) ->
    PieceLength = maps:get(<<"piece length">>, Data),
    Pieces = maps:get(<<"pieces">>, Data),
    PieceList = get_piece_list(Pieces),
    Private = maps:get(<<"private">>, Data, 0),
    InfoData = get_info_data(Data),
    #info{piece_length=PieceLength,
          piece_list=PieceList,
          private=Private,
          data = InfoData}.

-spec get_info_data(bdict()) -> single_file() | multi_file().
get_info_data(#{<<"name">> :=Name, <<"files">> := Files}=_Data) ->
    #multi_file{name=Name, files=Files};
get_info_data(#{<<"name">> := Name, <<"length">> := Length}=Data) ->
    Md5sum = maps:get(<<"md5sum">>, Data, undefined),
    #single_file{name=Name, length=Length, md5sum=Md5sum}.

-spec get_piece_list(binary()) -> list(url_encoded_hash()).
get_piece_list(Pieces) ->
    PieceCount = size(Pieces) div 20,
    get_piece_list(Pieces, PieceCount, []).

get_piece_list(_Pieces, 0, Acc) ->
    lists:reverse(Acc);
get_piece_list(<<Hash:20/bytes, Rest/binary>>, N, Acc) ->
    get_piece_list(Rest, N-1, [tent_utils:urlencode(Hash, []) | Acc]).
