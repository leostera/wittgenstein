-module(fdb_kafka_producer).

-export([start_inbound/0, start_outbound/0]).
-export([state_facts/1, project_entity/1]).

start_inbound() ->
  ok = brod:start_producer(fdb_kafka:client_id(), fdb_kafka:inbound_topic(), []).

start_outbound() ->
  ok = brod:start_producer(fdb_kafka:client_id(), fdb_kafka:outbound_topic(), []).

state_facts(Facts) ->
  [FirstFact|_] = Facts,
  Binary = erlang:term_to_binary(Facts),
  BatchKey = fdb_fact:uri(FirstFact),
  {ok, _Ref} = brod:produce(
                 fdb_kafka:client_id(),
                 fdb_kafka:inbound_topic(),
                 _Partition = 0,
                 BatchKey,
                 Binary),
  ok.

project_entity(Entity) ->
  Binary = jiffy:encode(Entity),
  {ok, _Ref} = brod:produce(
                 fdb_kafka:client_id(),
                 fdb_kafka:outbound_topic(),
                 _Partition = 0,
                 fdb_entity:uri(Entity),
                 Binary),
  ok.
