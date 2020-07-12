use wittgenstein::fact_db_client::FactDbClient;
use wittgenstein::ProjectionDescription;

use elasticsearch::http::transport::Transport;
use elasticsearch::Elasticsearch;
use elasticsearch::IndexParts;
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

    #[structopt(short = "u", long = "factdb-url", name = "FACTDB_URL")]
    factdb_url: String,
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let opt = Opt::from_args();

    println!("Running arguments =======================");
    println!("{:#?}", opt);
    println!("=======================");

    let mut fact_client = FactDbClient::connect(opt.factdb_url).await?;

    let transport = Transport::single_node(&opt.elasticsearch_url)?;
    let es_client = Elasticsearch::new(transport);

    let response = fact_client.project(ProjectionDescription {}).await?;
    let mut inbound = response.into_inner();

    while let Some(reply) = inbound.message().await? {
        let now = Instant::now();
        let index = IndexParts::IndexId(&opt.index_name, &reply.entity_uri);
        es_client.index(index).body(reply.fields).send().await?;
        let delta = Instant::now().duration_since(now).as_millis();
        println!("indexed {} in {}ms", reply.entity_uri, delta);
    }

    Ok(())
}
