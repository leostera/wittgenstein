-module(fdb_kafka_subscriber).

-include_lib("brod/include/brod.hrl").

-export([start/0]).
-export([init/2, handle_message/2]).

init(_Topic, _Args) -> {ok, none}.

handle_message(MessageSet, State) ->
  #kafka_message_set{ messages = Messages } = MessageSet,
  case (catch do_handle_message(Messages)) of
    {'EXIT', Reason} ->
      io:format("subscriber: crashed handling message: ~p", [Reason]),
      {ok, ack, State};
    ok ->
      {ok, ack, State}
  end.

do_handle_message(Messages) ->
  Batches = lists:map(fun (#kafka_message{ value = Value }) ->
                        erlang:binary_to_term(Value)
                    end,
                    Messages),

  lists:foreach(fun (Facts) ->
    ok = fdb_store:store_facts(Facts),

    T0 = erlang:system_time(),
    %Uris = maps:keys(lists:foldl(fun (F, Acc) -> Acc#{ F => ok } end, #{}, Facts)),
    UriSet = sets:from_list([ fdb_fact:entity_uri(F) || F <- Facts ]),
    Uris = sets:to_list(UriSet),
    T1 = erlang:system_time() - T0,
    io:format("~p | ts=~p extracted ~p uris for projection in ~pms\n",
              [ node(), erlang:system_time(), length(Uris), T1 / 1000000.0]),

    ok = factdb:project(Uris)
  end, Batches).

start() ->
  {ok, _SubscriberCoordinator} = brod:start_link_group_subscriber_v2(#{
    client => fdb_kafka:client_id(),
    group_id => <<"wittgenstein.factdb.fact_subscriber">>,
    topics => [fdb_kafka:inbound_topic()],
    cb_module => ?MODULE,
    consumer_config => [ {begin_offset, latest}
                       , {prefetch_count, 100}
                       ]
   }),
  ok.
