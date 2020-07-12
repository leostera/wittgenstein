-module(fdb_kafka).

-export([setup_client/0, setup_subscribers/0, setup_producers/0, ensure_topics/0]).
-export([client_id/0, partitions/0, inbound_topic/0, outbound_topic/0]).

inbound_topic() -> <<"wittgenstein.factdb.inbound">>.
outbound_topic() -> <<"wittgenstein.factdb.outbound">>.

client_id() -> factdb_kafka_client_0.

partitions() -> lists:seq(0, 10).

hosts() -> [{"kafka-0.kafka.foundation.svc.cluster.local", 9092}
           ,{"kafka-1.kafka.foundation.svc.cluster.local", 9092}
           ,{"kafka-2.kafka.foundation.svc.cluster.local", 9092}
           ].

setup_subscribers() ->
  {ok, _} = fdb_kafka_subscriber:start(),
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

topic_descriptions() ->
  [ #{ config_entries => [ #{ config_name  => <<"cleanup.policy">>
                            , config_value => "compact"
                            }
                         ],
       num_partitions => length(partitions()),
       replica_assignment => [],
       replication_factor => 1,
       topic => inbound_topic()
     }
  , #{ config_entries => [ #{ config_name  => <<"cleanup.policy">>
                            , config_value => "compact"
                            }
                         ],
       num_partitions => length(partitions()),
       replica_assignment => [],
       replication_factor => 1,
       topic => outbound_topic()
     }
  ].
