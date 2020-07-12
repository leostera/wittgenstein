# Wittgenstein

A fact-driven eventually consistent real-time semantic database.

## Model

Facts are tuples including:
* a source uri
* an entity uri
* a field uri (aka, role or property uri)
* a value (aka, object)
* a timestamp when it occurred

Facts form a total order using the grouping of (entity, field, timestamp,
source) so that for every (entity, field) pair there is a deterministic final
value according to the total ordering of the timestamp.

When two timestamps are exactly equal, the source is used to determine which
value is more authoritative.

Entities are the result of combining facts related to a particular Entity URI
into a single map of fields to values. They hold no purpose other than to ease
the consumption of the information stored as facts.

## System Properties

For every fact that is stated, a consolidation of all the existing facts for a
given _entity_ will be _eventually projected_ onto the existing subscribers.

How fast depends on the implementation, so long at it is guaranteed to be
projected.
