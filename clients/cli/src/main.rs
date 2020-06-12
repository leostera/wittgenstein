use wittgenstein::wittgenstein_client::WittgensteinClient;
use wittgenstein::StateFactRequest;

pub mod wittgenstein {
    tonic::include_proto!("dev.abstractmachines.wittgenstein");
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut client = WittgensteinClient::connect("http://0.0.0.0:50051").await?;

    let request = tonic::Request::new(StateFactRequest {
        entity_uri: "disney:character:test".into(),
        source_uri: "wittgenstein:system:cli".into(),
        field: "is_working".into(),
        value: "yasss".into(),
    });

    let response = client.state_fact(request).await?;

    println!("RESPONSE={:?}", response);

    Ok(())
}
