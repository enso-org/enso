# Language Server Documentation
The Enso Language Server is responsible for providing language services to the
Enso IDE (and other clients). This mainly involves speaking the Enso protocol
and orchestrating the runtime in response to this. It is responsible for:

- **Introspection Services:** Giving clients the ability to observe information
  about their running code including values, types, profiling information, and
  debugging.
- **Code Execution:** The ability for clients to execute arbitrary Enso code in
  arbitrary scopes. This can be used in conjunction with the above to provide
  a REPL with an integrated debugger.
- **Code Completion:** Sophisticated completion functionality that refines
  suggestions intelligently based on context.
- **Node Management:** Tracking and providing the language server's internal
  node representation of the Enso program.
- **Code Analysis:** Analysis functionality for Enso code (e.g. find usages,
  jump-to-definition, and so on).
- **Refactoring:** Refactoring functionality for Enso code (e.g. rename, move,
  extract, and so on).
- **Type Interactions:** Features for type-driven-development that allow users
  to interact with the types of their programs.

This folder contains all documentation pertaining to the Language Server.

