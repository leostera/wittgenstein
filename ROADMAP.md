# Roadmap

FactDB:
- require facts to have a microsecond timestamp
- use fact timestamp in column value to let cassandra determine who won
- support more kinds of values:
  - simple literals - DONE
  - language tagged literals
  - type tagged literals
- honor OWL cardinality
- BUG: let multiple projections subscribe via grpc separately
- scale horizontally
- instrument

Tools:
- fact tool:
  - timestamp facts automatically
- filestore importer:
  - 

---

Questions:
- Can we prove OWL DL compliance?

Spec:
- figure out what to model

---

CLI:
- Add CLI support for fetching entities - DONE
- Add CLI support for stating facts via input parameters - DONE

Core:
- Add Tracing to the entire library
- Write tests to get coverage close to total
- Make it distributed, so we can run several nodes in a cluster and increase
  throughput
- Writing the core in Erlang, and making the Elixir API on top of it - DONE
- Make `state/1` async - DONE

- how to deal with ontology definitions?
  should there be a way to define an ontology beforehand?
  should there be an _open_ ontology?
  what happens when we try to state facts that do not fit the ontology?


  // idempotent
  rpc LoadOntologyFromRDF(RDF) -> Ok | Err
  rpc GetOntologyByName(name) -> Ontology



GRPC:
- Look into Ghz to benchmark performance from the gRPC api perspective

Deployment:
- Containerize so its a `docker run wittgenstein` away

Utilities for adoption:
- Making a macro that turns an Ecto changeset into namespaced facts for an
  entity?
