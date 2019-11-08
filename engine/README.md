# The Enso Engine
The Enso engine is the codebase responsible for compiling and executing Enso
code, as well as providing language server functionality to users of the 
language. It is subdivided into two major components:

- [Runtime](#runtime)
- [Language Server](#language-server)

## Runtime
The Enso [runtime](/runtime) is responsible for the actual execution of Enso
code. This means that it encompasses the following functionality:

- **Parsing:** Taking Enso code as input and generating an AST that maintains a
  sophisticated set of information about the input.
- **Desugaring:** Reducing the user-facing Enso code into a simplified language
  known as `Core`.
- **Type Inference:** Inferring the types of bindings in the user's code.
- **Type Checking:** Checking that the inferred and provided types for bindings
  match up across the codebase.
- **Optimisation:** Static optimisation processes to improve the performance of
  the user's program.
- **Code Execution:** Actually running the Enso code.
- **Introspection Hooks:** Providing hooks into the running code to allow the
  language server to inspect information about the code as it runs.

## Language Server
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
