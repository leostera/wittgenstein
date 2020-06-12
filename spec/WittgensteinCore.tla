The core of Wittgenstein is a very small eventually consistent graph that takes
for inputs triplets that indicate what 2 nodes are connected and the label of
the edge.

The terminology used here is "Fact" to refer to a triplet, and "Entity" to refer
to a collection of triplets that may override each other to form a final graph.
That is, an Entity is a graph in itself.

There is also a graph of URIs that connects entities with each other (and from
which all the facts hinge to consolidate a specific entity).

The semantics of the system are:

1. Stating a fact means that some time in the future that entity will be projected
2. Stating a fact means that it is possible that the newly consolidated entity reflects
   that fact.
3. An entity is the consolidation of all the facts that have its URI for a target.

-------------------------- MODULE WittgensteinCore --------------------------


=============================================================================
\* Modification History
\* Last modified Sat Jun 13 00:33:55 CEST 2020 by ostera
\* Created Sat Jun 13 00:29:42 CEST 2020 by ostera
