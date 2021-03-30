-module(fdb_kafka_projection_subscriber).

-include_lib("brod/include/brod.hrl").

-export([start/0]).
-export([init/2, handle_message/3]).

init(_Topic, Pid) when is_pid(Pid) ->
  {ok, [], Pid}.

handle_message(_Partition, MessageSet, Pid) ->
  #kafka_message_set{ messages = Messages } = MessageSet,
  case (catch do_handle_message(Messages, Pid)) of
    {'EXIT', Reason} ->
      io:format("projection subscriber: crashed handling message: ~p", [Reason]);
    _ -> ok
  end,
  {ok, ack, Pid}.

do_handle_message(Messages, Pid) ->
  lists:foreach(fun (#kafka_message{ value = Value }) ->
                    Map = jiffy:decode(Value, [return_maps]),
                    Pid ! {entity, Map}
                end,
                Messages).

start() ->
  {ok, _} = brod_topic_subscriber:start_link(#{
    client => fdb_kafka:client_id(),
    topic => fdb_kafka:outbound_topic(),
    cb_module => ?MODULE,
    consumer_config => [{begin_offset, latest}],
    init_data => self()
   }),
  ok.
