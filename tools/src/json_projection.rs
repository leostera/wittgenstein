use wittgenstein::fact_db_client::FactDbClient;
use wittgenstein::ProjectionDescription;

use std::error::Error;
use structopt::StructOpt;
use tonic::transport::Channel;

pub mod wittgenstein {
    tonic::include_proto!("dev.abstractmachines.wittgenstein");
}

#[derive(StructOpt, Debug, Clone)]
#[structopt(name = "json-projection")]
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

    let mut client = FactDbClient::connect(opt.factdb_url).await?;

    stream_projected_entities(&mut client).await?;

    Ok(())
}

async fn stream_projected_entities(
    client: &mut FactDbClient<Channel>,
) -> Result<(), Box<dyn Error>> {
    let response = client.project(ProjectionDescription {}).await?;
    let mut inbound = response.into_inner();

    while let Some(reply) = inbound.message().await? {
        println!("{:?}\n", reply.fields);
    }

    Ok(())
}
