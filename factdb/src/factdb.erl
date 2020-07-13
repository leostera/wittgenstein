-module(factdb).

-export([start/0]).

-export([project/1, state/1]).
-export([subscribe_to_projection/0, entity_by_uri/1]).

start() ->
  ok = fdb_store:setup_cluster(),
  ok = fdb_store:ensure_tables(),
  ok = fdb_kafka:setup_client(),
  ok = fdb_kafka:ensure_topics(),
  ok = fdb_kafka:setup_producers(),
  ok = fdb_kafka:setup_subscribers(),
  ok.

entity_by_uri(Uri) ->
  Entity = fdb_store:fetch_consolidated_entity(Uri),
  {ok, Entity}.

state(Facts) ->
  ok = fdb_kafka_producer:state_facts(Facts),
  {ok, [ fdb_fact:uri(F) || F <- Facts ]}.

project(Uris) ->
  lists:foreach(fun (Uri) ->
                    {ok, Entity} = factdb:entity_by_uri(Uri),
                    fdb_kafka_producer:project_entity(Entity)
                end, Uris).

subscribe_to_projection() ->
  fdb_kafka_projection_subscriber:start().
