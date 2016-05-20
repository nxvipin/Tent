-module(tent_tracker).
-include("tent.hrl").

-export([start/2,
         stop/2,
         complete/2,
         request/2]).

start(#metainfo{}=MetaInfo, #context{}=Context) ->
    request(MetaInfo, Context, started).
stop(#metainfo{}=MetaInfo, #context{}=Context) ->
    request(MetaInfo, Context, stopped).
complete(#metainfo{}=MetaInfo, #context{}=Context) ->
    request(MetaInfo, Context, completed).
request(#metainfo{}=MetaInfo, #context{}=Context) ->
    request(MetaInfo, Context, undefined).

request(#metainfo{announce=AnnounceURL, info_hash=InfoHash}=Metainfo,
        #context{}=Context, Event) ->
    RequestParams = make_request_params(InfoHash, Context, Event),
    RequestURL = make_request_url(AnnounceURL, RequestParams).

make_request_params(InfoHash, #context{peer_id=PeerId,
                                       port=Port,
                                       uploaded=Uploaded,
                                       downloaded=Downloaded,
                                       left=Left,
                                       compact=Compact,
                                       no_peer_id=NoPeerID,
                                       ip=IP,
                                       numwant=NumWant,
                                       key=Key,
                                       tracker_id=TrackerID}, Event) ->

    Params0 = [{<<"info_hash">>, InfoHash},
               {<<"peer_id">>, PeerId},
               {<<"port">>, integer_to_binary(Port)},
               {<<"uploaded">>, integer_to_binary(Uploaded)},
               {<<"downloaded">>, integer_to_binary(Downloaded)},
               {<<"left">>, integer_to_binary(Left)},
               {<<"compact">>, integer_to_binary(Compact)},
               {<<"no_peer_id">>, binary_boolean(NoPeerID)},
               {<<"num_want">>, integer_to_binary(NumWant)}],

    Params0 ++ ip_opt(IP) ++ key_opt(Key) ++ tracker_id_opt(TrackerID) ++
        event_opt(Event).

make_request_url(BaseURL, Params) when is_binary(BaseURL)->
    case binary:match(BaseURL, <<"?">>) of
        nomatch ->
            make_request_url([<<"?">>, BaseURL], Params);
        {Prev, Size} when size(BaseURL) > Prev + Size->
            make_request_url([<<"&">>, BaseURL], Params);
        _ ->
            make_request_url([BaseURL], Params)
    end;
make_request_url([_H|_T]=BaseURL, [{Key, Value} | Rest]) ->
    make_request_url([<<"&">>, Value, <<"=">>, Key |BaseURL], Rest);
make_request_url([Head | BaseURL], []) ->
    iolist_to_binary(lists:reverse(BaseURL)).

binary_boolean(true) ->
    <<"true">>;
binary_boolean(false) ->
    <<"false">>.

ip_opt(undefined) ->
    [];
ip_opt(IP) ->
    [{<<"ip">>, IP}].

key_opt(undefined) ->
    [];
key_opt(Key) ->
    [{<<"key">>, Key}].

tracker_id_opt(undefined) ->
    [];
tracker_id_opt(TrackerID) ->
    [{<<"tracker_id">>, TrackerID}].

event_opt(undefined) ->
    [];
event_opt(started) ->
    [{<<"event">>, <<"started">>}];
event_opt(stopped) ->
    [{<<"event">>, <<"stopped">>}];
event_opt(completed) ->
    [{<<"event">>, <<"completed">>}].