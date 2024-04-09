# Providers

This folder contains [Vue providers](provider).

All providers MUST be at the top level - so either `./providers/myProvider.ts`,
or `./providers/myProvider/index.ts`.

Each provider MAY have dependencies specific to itself, which MUST NOT be at the
top level - so `./providers/myProvider/myDependency.ts`.

[provider]: https://vuejs.org/guide/components/provide-inject
