use wittgenstein::fact_db_client::FactDbClient;
use wittgenstein::ProjectionDescription;

use std::error::Error;
use tonic::transport::Channel;

pub mod wittgenstein {
    tonic::include_proto!("dev.abstractmachines.wittgenstein");
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut client = FactDbClient::connect("http://0.0.0.0:50051").await?;

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
