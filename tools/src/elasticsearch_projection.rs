use elasticsearch::http::transport::Transport;
use elasticsearch::Elasticsearch;
use elasticsearch::IndexParts;
use rdkafka::config::ClientConfig;
use rdkafka::consumer::{BaseConsumer, Consumer};
use rdkafka::Message;
use serde_json::Value;
use std::time::Instant;
use structopt::StructOpt;

pub mod wittgenstein {
    tonic::include_proto!("dev.abstractmachines.wittgenstein");
}

#[derive(StructOpt, Debug)]
#[structopt(name = "elasticsearch-projection")]
struct Opt {
    #[structopt(short = "u", long = "elasticsearch-url", name = "ELASTICSEARCH_URL")]
    elasticsearch_url: String,

    #[structopt(short = "i", long = "index-name", name = "INDEX_NAME")]
    index_name: String,

    #[structopt(short = "f", long = "factdb-url", name = "FACTDB_URL")]
    factdb_url: String,

    #[structopt(short = "g", long = "kafka-group-id", name = "KAFKA_GROUP_ID")]
    kafka_group_id: String,
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let opt = Opt::from_args();

    let hostname = std::env::var("HOSTNAME").unwrap();

    println!("Running arguments =======================");
    println!("{:#?}", opt);
    println!("hostname: {}", hostname);
    println!("=======================");

    let consumer: BaseConsumer = ClientConfig::new()
        .set("group.id", &opt.kafka_group_id)
        .set(
            "bootstrap.servers",
            &[
                "kafka-0.kafka.foundation.svc.cluster.local",
                "kafka-1.kafka.foundation.svc.cluster.local",
                "kafka-2.kafka.foundation.svc.cluster.local",
            ]
            .join(","),
        )
        .set("auto.offset.reset", "earliest")
        .set("enable.auto.commit", "false")
        .set("session.timeout.ms", "6000")
        .create()
        .expect("Could not create Kafka Consumer");

    consumer
        .subscribe(&["wittgenstein.factdb.outbound"])
        .expect("Could not subscribe to minio notifications topic!");

    let transport = Transport::single_node(&opt.elasticsearch_url)?;
    let es_client = Elasticsearch::new(transport);

    for borrowed_message in consumer.iter() {
        match borrowed_message {
            Ok(borrowed_message) => {
                let message = borrowed_message.detach();
                let json = std::str::from_utf8(message.payload().unwrap()).unwrap();
                let payload = serde_json::from_str::<Value>(&json).unwrap();
                let now = Instant::now();
                let uri = payload["uri"].to_string().clone();
                let index = IndexParts::IndexId(&opt.index_name, &uri);
                let response = es_client.index(index).body(payload.clone()).send().await?;

                let successful = response.status_code().is_success();
                let delta = Instant::now().duration_since(now).as_millis();
                if successful {
                    println!("{} | indexed {} in {}ms", &hostname, payload["uri"], delta);
                } else {
                    println!(
                        "error indexing {}: {:?}",
                        payload["uri"],
                        response.json::<Value>().await?
                    );
                }
            }
            e => println!("skipping errored message {:?}", e),
        }
    }

    Ok(())
}
