---
layout: developer-doc
title: Searcher Panel In Graph Editor
category: product
tags: [product]
---

# Searcher Panel In Graph Editor

### Behaviour

The Searcher Panel can be brought to the screen in two different ways:

- when user starts to edit node's expression - the node becomes Searcher input
  and panel appears below,
- when user press tab with mouse over Graph Editor Panel - the new Searcher
  input appears with Searcher panel below. Additionally, if there is exactly one
  node selected, the connection is displayed between the selected node and the
  Searcher input.

### Suggestion of Node Expressions

The suggestion list is obtained from the Engine using `search/completion` method
of Language Server. The parameters of the call depend on the Current searcher
input, suggestions picked suggestions so far, and if we are adding a node
connected to selection. The current implementation can be found in
`ide::controller::searcher` module.

### Suggestion Database

The `search/completion` method of Language Server returns a list of keys to the
Suggestion Database instead of the whole entries. The database is retrieved by
IDE on Project opening, and keeps it up to date. The Database is implemented in
`ide::model::suggestion_database` module.
