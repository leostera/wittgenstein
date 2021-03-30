-module(factdb).

-export([start/0]).

-export([project/1, state/1]).
-export([subscribe_to_projection/0, entity_by_uri/1]).
-export([batch_facts/2]).

start() ->
  ok = fdb_store:setup_cluster(),
  ok = fdb_store:ensure_tables(),
  ok = fdb_kafka:setup_client(),
  ok = fdb_kafka:ensure_topics(),
  ok = fdb_kafka:setup_producers(),
  ok = fdb_kafka:setup_subscribers(),
  ok.

-spec entity_by_uri(fdb_uri:t()) -> {ok, map()} | {error, could_not_find_entity}.
entity_by_uri(Uri) ->
  Entity = fdb_store:fetch_consolidated_entity(Uri),
  {ok, Entity}.

state(Facts) ->
  Batches = batch_facts(Facts, _BatchSize = 100),
  ok = lists:foreach(fun do_state/1, Batches),
  {ok, [ fdb_fact:uri(F) || F <- Facts ]}.

do_state(Facts) ->
  ok = fdb_kafka_producer:state_facts(Facts),
  {ok, [ fdb_fact:uri(F) || F <- Facts ]}.

batch_facts([], _) -> [];
batch_facts(Facts, BatchSize) -> do_batch_facts(Facts, BatchSize, [], []).

do_batch_facts([], Size, Last, Acc) -> [Last | Acc];
do_batch_facts([X|Xs], Size, Curr, Acc) when length(Curr) < Size ->
  do_batch_facts(Xs, Size, [X|Curr], Acc);
do_batch_facts(Xs, Size, Curr, Acc) when length(Curr) == Size ->
  do_batch_facts(Xs, Size, [], [Curr | Acc]).

project(Uris) ->
  Entities = lists:filtermap(fun (Uri) ->
                                 case factdb:entity_by_uri(Uri) of
                                   {ok, Entity} -> {true,  Entity};
                                   _ -> false
                                 end
                             end, Uris),
  ok = fdb_kafka_producer:project_entities(Entities),
  ok.

subscribe_to_projection() ->
  fdb_kafka_projection_subscriber:start().
