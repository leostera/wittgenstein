# Wittgenstein

A fact-driven eventually consistent real-time semantic database.

1) Define your fact consolidation logic and entity projections via config

```ex
config Wittgenstein, Consolidation,
  strategy: MyApp.DynamicConsolidation 

config Wittgenstein, Projections, [
  MyApp.Search.Projection,
  MyApp.GraphQL.Projection,
  MyApp.Reasoning.Projection,
  MyApp.Statistics.Projection
]
```

2) Start it up

```
  extra_applications: [:wittgenstein]
```
