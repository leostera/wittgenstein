-module(fdb_kafka_producer).

-export([start_inbound/0, start_outbound/0]).
-export([state_facts/1, project_entity/1]).

start_inbound() ->
  ok = brod:start_producer(fdb_kafka:client_id(), fdb_kafka:inbound_topic(), []).

start_outbound() ->
  ok = brod:start_producer(fdb_kafka:client_id(), fdb_kafka:outbound_topic(), []).

state_facts(Facts) ->
  T0 = erlang:system_time(),

  [FirstFact|_] = Facts,
  Binary = erlang:term_to_binary(Facts),
  BatchKey = fdb_fact:uri(FirstFact),
  {ok, _Ref} = brod:produce(
                 fdb_kafka:client_id(),
                 fdb_kafka:inbound_topic(),
                 _Partition = fdb_kafka:random_partition(),
                 BatchKey,
                 Binary),

  T1 = erlang:system_time() - T0,
  io:format("ts=~p produced batch with ~p facts in ~pms\n",
            [ erlang:system_time(), length(Facts), T1 / 1000000.0]),
  ok.

project_entity(Entity) ->
  Binary = jiffy:encode(Entity),
  {ok, _Ref} = brod:produce(
                 fdb_kafka:client_id(),
                 fdb_kafka:outbound_topic(),
                 _Partition = fdb_kafka:random_partition(),
                 fdb_entity:uri(Entity),
                 Binary),
  ok.
