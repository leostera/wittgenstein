# FactDB
> A semantic, real-time, distributed, knowledge database.

FactDB is structured as:

* a set of Erlang applications to be embedded into your system

* a cli tool to generate code from an ontological definition of knowledge

* a set of GUI tools for creating ontologies, operating the Erlang systems, and
  exploring the data


The interactions between them are:

* the cli tool spits out code for handling, validating, and generating data that
  follows the ontology, both consolidated entities and single facts

* the Erlang applications work as a generic database with a framework for
  consolidating facts into entities and projecting entities; a pluggable set of
  ontologies is used for verifying the shape of the data, the consolidation
  rules, and the available projections
