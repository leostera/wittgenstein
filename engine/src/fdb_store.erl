-module(fdb_store).

-include_lib("cqerl/include/cqerl.hrl").

-export([
          ensure_tables/0
        , setup_cluster/0
        , store_facts/1
        , fetch_consolidated_entity/1
        ]).

%%%-------------------------------------------------------------------
%%% Setup Functions
%%%-------------------------------------------------------------------

setup_cluster() ->
  ok = cqerl_cluster:add_nodes([ "10.152.183.48" ],
                               [ {keyspace, wittgenstein} ]),
  ok.


ensure_tables() ->
  {ok, _} = run_query(query__create_table_facts_by_entity_field()),
  {ok, _} = run_query(query__create_table_facts()),
  ok.

%%%-------------------------------------------------------------------
%%% API Functions
%%%-------------------------------------------------------------------

store_facts(Facts) ->
  T0 = erlang:system_time(),
  {ok, Client} = cqerl:get_client(),
  lists:foreach(fun (Fact) ->
                    Q0 = query__insert_into_facts(Fact),
                    cqerl:send_query(Client, Q0),
                    Q1 = query__insert_into_facts_by_entity_field(Fact),
                    cqerl:send_query(Client, Q1)
                end, Facts),
  T1 = erlang:system_time() - T0,
  io:format("~p | ts=~p stored ~p facts in ~pms\n",
            [ node(), erlang:system_time(), length(Facts), T1 / 1000000.0]),
  ok.

fetch_consolidated_entity(EntityUri) ->
  {ok, Results} = run_query(query__select_facts_by_entity_uri(EntityUri)),
  Rows = cqerl:all_rows(Results),
  lists:foldl(fun ([{ field_uri, Field }, { value, Value }], Entity) ->
                  fdb_entity:set_field(Entity, Field, Value)
              end, fdb_entity:new(EntityUri), Rows).

%%%-------------------------------------------------------------------
%%% Private functions
%%%-------------------------------------------------------------------

run_query(Query) ->
  {ok, Client} = cqerl:get_client(),
  cqerl:run_query(Client, Query).

% query__create_keyspace_if_not_exists() ->
%   "CREATE KEYSPACE wittgenstein WITH replication = {'class': 'SimpleStrategy', 'replication_factor': 3 };".

query__select_facts_by_entity_uri(EntityUri) ->
  #cql_query{
     reusable = true,
     consistency = local_quorum,
     statement = "SELECT field_uri, value FROM facts_by_entity_field WHERE entity_uri = ?;",
     values = [{entity_uri, EntityUri}]
    }.

query__insert_into_facts(Fact) ->
  #cql_query {
     reusable = true,
     consistency = local_quorum,
     statement = "INSERT INTO facts (fact_uri, ts, source_uri, entity_uri, field_uri, value) VALUES ( ?, ?, ?, ?, ?, ? ) ;
             ",
     values = [ {fact_uri, fdb_fact:uri(Fact)}
              , {ts, fdb_fact:timestamp(Fact)}
              , {source_uri, fdb_fact:source_uri(Fact)}
              , {entity_uri, fdb_fact:entity_uri(Fact)}
              , {field_uri, fdb_fact:field_uri(Fact)}
              , {value, fdb_fact:value(Fact)}
              ]
    }.

query__insert_into_facts_by_entity_field(Fact) ->
  #cql_query {
     reusable = true,
     consistency = local_quorum,
     statement = "INSERT INTO facts_by_entity_field (entity_uri, field_uri, value) VALUES ( ?, ?, ? ) ;
             ",
     values = [ {entity_uri, fdb_fact:entity_uri(Fact)}
              , {field_uri, fdb_fact:field_uri(Fact)}
              , {value, fdb_fact:value(Fact)}
              ]
    }.

query__create_table_facts() ->
  "CREATE TABLE IF NOT EXISTS facts ( source_uri text, entity_uri text, field_uri text, ts timestamp, fact_uri text, value text, PRIMARY KEY ( (source_uri, fact_uri), ts, entity_uri, field_uri ) ) WITH COMPACT STORAGE ;".

query__create_table_facts_by_entity_field() ->
  "CREATE TABLE IF NOT EXISTS facts_by_entity_field ( entity_uri text, field_uri text, value text, PRIMARY KEY ( (entity_uri), field_uri ) ) WITH COMPACT STORAGE ;".
