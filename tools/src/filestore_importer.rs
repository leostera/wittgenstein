#![recursion_limit = "256"]

use wittgenstein::fact_db_client::FactDbClient;
use wittgenstein::StateFactRequest;

use rdkafka::config::ClientConfig;
use rdkafka::consumer::{BaseConsumer, Consumer};
use rdkafka::Message;
use structopt::StructOpt;
use uuid::Uuid;

pub mod wittgenstein {
    tonic::include_proto!("dev.abstractmachines.wittgenstein");
}

#[derive(StructOpt, Debug, Clone)]
#[structopt(name = "filestore-importer")]
struct Opt {
    #[structopt(short = "u", long = "factdb-url", name = "FACTDB_URL")]
    factdb_url: String,
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let opt = Opt::from_args();

    println!("Running arguments =======================");
    println!("{:#?}", opt);
    println!("=======================");

    let consumer: BaseConsumer = ClientConfig::new()
        .set("group.id", "0")
        .set("enable.partition.eof", "false")
        .set(
            "bootstrap.servers",
            &[
                "kafka-0.kafka.foundation.svc.cluster.local",
                "kafka-1.kafka.foundation.svc.cluster.local",
                "kafka-2.kafka.foundation.svc.cluster.local",
            ]
            .join(","),
        )
        .set("enable.auto.commit", "false")
        .set("session.timeout.ms", "6000")
        .create()
        .expect("Could not create Kafka Consumer");

    consumer
        .subscribe(&["foundation.minio.notifications"])
        .expect("Could not subscribe to minio notifications topic!");

    let mut client = FactDbClient::connect(opt.factdb_url).await?;

    for borrowed_message in consumer.iter() {
        match borrowed_message {
            Ok(borrowed_message) => {
                let message = borrowed_message.detach();
                let json = std::str::from_utf8(message.payload().unwrap()).unwrap();
                let payload = json::parse(&json).unwrap();

                let fact = StateFactRequest {
                    entity_uri: format!(
                        "https://wittgenstein.dev/2020/filestore/file#{}",
                        Uuid::new_v4().hyphenated()
                    ),
                    field_uri: "https://wittgenstein.dev/2020/filestore/s3_url".to_string(),
                    source_uri: "https://wittgenstein.dev/2020/tools#filestore_importer-0.0.0"
                        .to_string(),
                    value: format!("s3://{}", payload["Key"]),
                };
                println!("Producing fact: {:?}", fact);
                match client.state_fact(fact).await {
                    Ok(result) => println!("Fact {:?} was stated.", result),
                    err => println!("Skipping error: {:?}", err),
                };
            }
            e => println!("skipping errored message {:?}", e),
        }
    }

    Ok(())
}
