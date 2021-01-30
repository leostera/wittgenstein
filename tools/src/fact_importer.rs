#![recursion_limit = "256"]

use wittgenstein::fact_db_client::FactDbClient;
use wittgenstein::StateFactRequest;

use std::error::Error;
use std::fs::File;
use std::io::BufReader;
use std::path::PathBuf;

use structopt::StructOpt;
use tonic::transport::Channel;
use tonic::Request;

use rio_api::parser::TriplesParser;
use rio_turtle::TurtleError;
use rio_turtle::TurtleParser;

pub mod wittgenstein {
    tonic::include_proto!("dev.abstractmachines.wittgenstein");
}

#[derive(StructOpt, Debug, Clone)]
#[structopt(name = "fact-importer")]
struct Opt {
    #[structopt(short = "u", long = "factdb-url", name = "FACTDB_URL")]
    factdb_url: String,

    #[structopt(
        short = "f",
        long = "file-path",
        name = "FILE_PATH",
        parse(from_os_str)
    )]
    file_path: PathBuf,

    #[structopt(short = "s", long = "source-uri", name = "SOURCE_URI")]
    source_uri: String,
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let opt = Opt::from_args();
    println!("Running arguments =======================");
    println!("{:#?}", opt);
    println!("=======================");

    let mut client = FactDbClient::connect(opt.factdb_url.clone()).await?;

    state_facts(&mut client, opt.clone()).await?;

    Ok(())
}

async fn state_facts(client: &mut FactDbClient<Channel>, opt: Opt) -> Result<(), Box<dyn Error>> {
    let file = File::open(&opt.file_path)?;
    let reader = BufReader::new(file);

    let outbound = async_stream::stream! {
        let triples = TurtleParser::new(reader, "")
            .unwrap()
            .into_iter( |mut t| {
                let f = StateFactRequest {
                    entity_uri: t.subject.to_string().replace("<", "").replace(">", ""),
                    field_uri: t.predicate.iri.to_string(),
                    source_uri: (&opt.source_uri).to_string(),
                    value: t.object.to_string(),
                };
                Ok(f) as Result<StateFactRequest, TurtleError>
            });

        for fact in triples {
            match fact {
                Ok(f) => yield f,
                err => print!("\nError: {:?}", err)
            };
        };

        ()
    };

    let response = client.state_facts(Request::new(outbound)).await?;
    println!("\nImported {:?} facts.", response);
    Ok(())
}
