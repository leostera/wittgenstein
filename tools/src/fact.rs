use wittgenstein::fact_db_client::FactDbClient;
use wittgenstein::StateFactRequest;

use structopt::StructOpt;

pub mod wittgenstein {
    tonic::include_proto!("dev.abstractmachines.wittgenstein");
}

#[derive(StructOpt, Debug)]
#[structopt(name = "fact")]
struct Opt {
    #[structopt(short = "s", long = "source-uri", name = "SOURCE_URI")]
    source_uri: String,

    #[structopt(short = "f", long = "field-uri", name = "FIELD_URI")]
    field_uri: String,

    #[structopt(short = "e", long = "entity-uri", name = "ENTITY_URI")]
    entity_uri: String,

    #[structopt(short = "v", long = "value", name = "VALUe")]
    value: String,
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let opt = Opt::from_args();

    println!("Running arguments =======================");
    println!("{:#?}", opt);
    println!("=======================");

    let fact = StateFactRequest {
        entity_uri: opt.entity_uri,
        field_uri: opt.field_uri,
        source_uri: opt.source_uri,
        value: opt.value,
    };

    let mut client = FactDbClient::connect("http://0.0.0.0:50051").await?;

    client.state_fact(fact).await?;

    Ok(())
}
