# Enso Language Server
The Enso Language Server is responsibile for providing a remote-communication
protocol for the runtime, exposing many of its features to the users. In
addition it provides the backing service for much of the IDE functionality
associated with the language. It encompasses the following functionality:

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

