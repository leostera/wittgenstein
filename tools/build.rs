fn main() -> Result<(), Box<dyn std::error::Error>> {
    tonic_build::compile_protos("./schemata/protobuf/wittgenstein/factdb.proto")?;
    Ok(())
}
