-module(fdb_uri).

-export([new_fact/0]).

-export_type([t/0]).

-type t() :: binary().

new_fact() ->
  Uuid = binary:list_to_bin(uuid:uuid_to_string(uuid:get_v4())),
  << "https://abstractmachines.dev/wittgenstein/2020/Fact#", Uuid/binary >>.
