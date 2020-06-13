# Roadmap

Questions:
- Can we prove OWL DL compliance?

Spec:
- figure out what to model

Clients/CLI:
- Add CLI support for fetching entities
- Add CLI support for stating facts via input parameters

Services/Core:
- Add Tracing to the entire library
- Write tests to get coverage close to total
- Make it distributed, so we can run several nodes in a cluster and increase
  throughput
- Writing the core in Erlang, and making the Elixir API on top of it
- Make `state/1` async

Services/GRPC:
- Look into Ghz to benchmark performance from the gRPC api perspective

Deployment:
- Containerize so its a `docker run wittgenstein` away

Utilities for adoption:
- Making a macro that turns an Ecto changeset into namespaced facts for an
  entity?
