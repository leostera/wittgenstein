# Roadmap

FactDB:

- [x] store all facts
- [x] require facts to have a nanosecond timestamp
- [ ] use fact timestamp in column value to let cassandra determine who won
- [ ] support more kinds of values:
      - [ ] simple literals - DONE
      - [ ] language tagged literals
      - [ ] type tagged literals
      - [ ] collections?
- [ ] honor OWL cardinality
- [x] BUG: let multiple projections subscribe via grpc separately
     - NOTE: we're just exposing the outbound kafka topic now
- [x] scale horizontally
- [ ] instrument
- [ ] fix Supervision tree to keep all connections (kafka, cassandra) alive

Tools:
- fact tool:
  - timestamp facts automatically
- filestore importer:
  - check why we're producing facts in chukns of ~60k facts, is it the tool?
- filestore verify:
  - parse the whole file and spit out the errors to be fixed all at once
- github repo analyzer:
  - dump an entire repository as documents in the CreativeWorks ontology
- github repo updater:
  - syn


NLP:
- define ontology for analyzable documents: can we reuse CreativeWork from Schema.org?
- prototype:
  - given a CreativeWork, extract the entities as _things_ that belong to that project's ontology

μWittgenstein:
- create a local knowledgebase at <path/to/project>
- provide a binary `wittgenstein` to interact with projects
- examples:

  1. wittgenstein new Dota2Ontology
  2. ls
   > .wittgenstein/
  3. wittgenstein analyze:
     runs a local analysis of all the documnets in the folder
     outputs a bunch of files in .wittgenstein (facts, entities)

---

Questions:
- Can we prove OWL DL compliance?
- Can we do an embedded tiny-fact engine?


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
