# Ideas

## Stating a Fact

## Creating a new Ontology

Ontologies should be persisted in disk, and the cli tool should be used to
generate the appropriate library code for utilizing them in the running system.

However, _defining_ the ontology itself may be tricky if you're not used to
writing Turtle files.

I'd like to create an ontology in just a few steps, making it trivial to iterate
on it and see the results of a change in live data:

1. Click on New Ontology, write the name "Paleontology"
2. A small editor in-browser lets me create new Classes, with new attributes,
   and forces me to document them. I can visually explore the relations between
   them.
3. Once I'm done editing, I can save it and _preview_ it with a single click.

The _previewing_ process takes care of invocating the cli tool, which does the
code generation, compiling the necessary code, and temporarily loading it into
the running system.

Immediately after this I should be able to go into any entity in the system, and
click on "Consolidate > "

## Exploring Entities

## Viewing an Entity in detail
