-module(tent_tracker).
-include("tent.hrl").

-export([start/2,
         stop/2,
         complete/2,
         request/2,
         do_request/1,
         get_tracker_response/1]).

start(#metainfo{}=MetaInfo, #context{}=Context) ->
    request(MetaInfo, Context, started).
stop(#metainfo{}=MetaInfo, #context{}=Context) ->
    request(MetaInfo, Context, stopped).
complete(#metainfo{}=MetaInfo, #context{}=Context) ->
    request(MetaInfo, Context, completed).
request(#metainfo{}=MetaInfo, #context{}=Context) ->
    request(MetaInfo, Context, undefined).

request(#metainfo{announce=AnnounceURL, info_hash=InfoHash}=_Metainfo,
        #context{}=Context, Event) ->
    RequestParams = make_request_params(InfoHash, Context, Event),
    RequestURL = make_request_url(AnnounceURL, RequestParams),
    lager:debug("Sending Request(~p) to Url: ~p", [Event, RequestURL]),
    case do_request(RequestURL) of
        {ok, Resp} ->
            get_tracker_response(Resp);
        Error ->
            lager:error("Tracker request failed with error: ~p", [Error]),
            ok
    end.

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
make_request_url([_Head | BaseURL], []) ->
    iolist_to_binary(lists:reverse(BaseURL)).

-spec do_request(tracker_url()) -> {ok, binary()} |
                                   {http_error, httpc:status_code(), binary()} |
                                   {error, any()}.
do_request(RequestURL0) ->
    RequestURL = binary_to_list(RequestURL0),
    case httpc:request(get, {RequestURL, []}, [], [{body_format, binary}]) of
        {ok, {{_V, 200, _R}, _H, Body}} ->
            lager:debug("Tracker Request Success"),
            {ok, Body};
        {ok, {{_V, StatusCode, _R}, _H, Body}} ->
            lager:error("Tracker Request HTTP Error(~p): ~p",
                        [StatusCode, Body]),
            {http_error, StatusCode, Body};
        {error, Reason} ->
            lager:error("Tracker Request Error: ~p", [Reason]),
            {error, Reason}
    end.

-spec get_tracker_response(binary()) -> {ok, tracker_response()} |
                                        {error, term()} |
                                        {tracker_error, term()}.
get_tracker_response(Resp) when is_binary(Resp) ->
    get_tracker_response(bencode:decode(Resp));
get_tracker_response({ok, #{<<"failure reason">> := Reason}=_Data}) ->
    {tracker_error, Reason};
get_tracker_response({ok, #{<<"interval">> := Interval,
                            <<"peers">> := [_H|_T]=Peers0}=Data}) ->

    Peers = peer_list_transform(Peers0),
    WarningMsg = maps:get(<<"warning message">>, Data, undefined),
    MinInterval = maps:get(<<"min interval">>, Data, undefined),
    TrackerID = maps:get(<<"tracker id">>, Data, undefined),
    Complete = maps:get(<<"complete">>, Data, undefined),
    Incomplete = maps:get(<<"incomplete">>, Data, undefined),

    {ok, #tracker_response{interval=Interval,
                           peers=Peers,
                           warning_message=WarningMsg,
                           min_interval=MinInterval,
                           tracker_id=TrackerID,
                           complete=Complete,
                           incomplete=Incomplete}};
get_tracker_response({ok, #{<<"interval">> := _Interval,
                            <<"peers">> := <<_Peers/binary>>}=_Data}) ->
    {error, "Binary Model Peer Unsupported"};
get_tracker_response({error, _Error}) ->
    {tracker_error, "Failed to decode tracker response"}.


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

peer_list_transform(PeerList) ->
    peer_list_transform(PeerList, []).
peer_list_transform([], Acc) ->
    Acc;
peer_list_transform([Peer | Rest], Acc) ->
    peer_list_transform(Rest, [peer_map_transform(Peer) | Acc]).

peer_map_transform(#{<<"ip">> := IP, <<"port">> := Port}=Peer) ->
    %% [TODO]: Aesthetics over Efficieny. Maybe reconsider (or not)?
    PeerID = maps:get(<<"peer id">>, Peer, undefined),
    #{peer_id => PeerID, ip => IP, port => Port}.
