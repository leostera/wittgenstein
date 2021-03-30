-module(fdb_kafka).

-export([setup_client/0, setup_subscribers/0, setup_producers/0, ensure_topics/0]).
-export([client_id/0, partitions/0, random_partition/0, partition_count/0, inbound_topic/0, outbound_topic/0]).

%%==============================================================================
%% Setup
%%==============================================================================

setup_subscribers() ->
  ok = fdb_kafka_subscriber:start(),
  ok.

setup_producers() ->
  ok = fdb_kafka_producer:start_inbound(),
  ok = fdb_kafka_producer:start_outbound().

setup_client() ->
  ClientConfig = [{reconnect_cool_down_seconds, 10}],
  ok = brod:start_client(hosts(), client_id(), ClientConfig).

ensure_topics() ->
  case brod:create_topics(hosts(), topic_descriptions(), #{timeout => 1000}) of
    ok -> ok;
    {error, topic_already_exists} -> ok
  end.

%%==============================================================================
%% Configuration
%%==============================================================================

inbound_topic() -> <<"wittgenstein.factdb.inbound">>.
outbound_topic() -> <<"wittgenstein.factdb.outbound">>.

client_id() -> factdb_kafka_client_0.

partitions() -> lists:seq(0, 9).

partition_count() -> length(partitions()).

random_partition() -> rand:uniform(partition_count() - 1).

hosts() -> [{"kafka-0.kafka.foundation.svc.cluster.local", 9092}
           ,{"kafka-1.kafka.foundation.svc.cluster.local", 9092}
           ,{"kafka-2.kafka.foundation.svc.cluster.local", 9092}
           ].

topic_descriptions() ->
  [ #{ config_entries => [ #{ config_name  => <<"cleanup.policy">>
                            , config_value => "compact"
                            }
                         ],
       num_partitions => partition_count(),
       replica_assignment => [],
       replication_factor => 10,
       topic => inbound_topic()
     }
  , #{ config_entries => [ #{ config_name  => <<"cleanup.policy">>
                            , config_value => "compact"
                            }
                         ],
       num_partitions => partition_count(),
       replica_assignment => [],
       replication_factor => 10,
       topic => outbound_topic()
     }
  ].
