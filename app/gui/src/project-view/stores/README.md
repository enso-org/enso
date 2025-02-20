# Stores

This folder contains [Pinia] stores.

All stores MUST be at the top level - so either `./stores/myStore.ts`, or
`./stores/myStore/index.ts`.

Each store MAY have dependencies specific to itself, which MUST NOT be at the
top level - so `./stores/myStore/myDependency.ts`.

[Pinia]: https://pinia.vuejs.org/
