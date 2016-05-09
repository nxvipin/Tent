-record(single_file, {name :: binary(),
                      length :: pos_integer(),
                      md5sum :: undefined | binary()}).

-record(multi_file, {name :: binary(),
                     files :: list(#{length => pos_integer(),
                                     md5sum => undefined | binary(),
                                     path => list(binary())})}).

-record(info, {piece_length :: pos_integer(),
               piece_list :: list(binary()),
               private :: 1 | 0,
               data :: single_file() | multi_file()}).

-record(metainfo, {announce :: binary(),
                   announce_list :: undefined | binary(),
                   creation_date :: undefined | pos_integer(),
                   comment :: undefined | binary(),
                   created_by :: undefined | binary(),
                   encoding :: undefined | binary(),
                   info :: info()}).


-type single_file() :: #single_file{}.
-type multi_file() :: #multi_file{}.
-type info() :: #info{}.
-type metainfo() :: #metainfo{}.
-type torrent_file() :: string().
-type url_encoded_hash() :: binary().
