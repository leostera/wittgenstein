-module(fdb_fact).

-export([ new/0
        , build/1
        , set_entity_uri/2
        , set_field_uri/2
        , set_source_uri/2
        , set_timestamp/2
        , set_value/2
        ]).

-export([ uri/1
        , entity_uri/1
        , field_uri/1
        , source_uri/1
        , timestamp/1
        , value/1
        ]).

-export_type([t/0]).

-type t() :: #{
        uri => binary(),
        source_uri => binary(),
        entity_uri => binary(),
        field_uri => binary(),
        value => binary(),
        ts => non_neg_integer()
       }.

new() -> #{ uri => fdb_uri:new_fact(), ts => erlang:system_time() }.

set_source_uri(Fact, X) when is_map(Fact) and is_binary(X) -> Fact#{ source_uri => X }.
set_entity_uri(Fact, X) when is_map(Fact) and is_binary(X) -> Fact#{ entity_uri => X }.
set_field_uri(Fact, X) when is_map(Fact) and is_binary(X) -> Fact#{ field_uri => X }.
set_value(Fact, X) when is_map(Fact) and is_binary(X) -> Fact#{ value => X }.
set_timestamp(Fact, X) when is_map(Fact) and is_integer(X) -> Fact#{ ts => X }.

uri(#{ uri := X }) -> X.
source_uri(#{ source_uri := X }) -> X.
entity_uri(#{ entity_uri := X }) -> X.
field_uri(#{ field_uri := X }) -> X.
value(#{ value := X }) -> X.
timestamp(#{ ts := X }) -> X.

build(#{ uri := Uri,
         source_uri := SourceUri,
         entity_uri := EntityUri,
         field_uri := FieldUri,
         value := Value,
         ts := Ts
       }=Fact) when
    is_binary(Uri) and
    is_binary(SourceUri) and
    is_binary(EntityUri) and
    is_binary(FieldUri) and
    is_binary(Value) and
    is_integer(Ts)
    -> {ok, Fact};
build(_) -> {error, invalid_fact}.
