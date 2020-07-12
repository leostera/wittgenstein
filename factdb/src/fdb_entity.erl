-module(fdb_entity).

-export([ new/1
        , set_field/3
        , uri/1
        ]).

new(Uri) -> #{ <<"uri">> => Uri }.

uri(#{ <<"uri">> := Uri }) -> Uri.

set_field(Entity, Field, Value) when is_binary(Field) -> Entity#{ Field => Value }.
