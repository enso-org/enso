# Enso Runtime
The Enso runtime refers to both the compiler and the optimising JIT runtime for
Enso. While this might seem like a strange choice, it makes sense internally as
the components are integrated to a level not seen in most languages. It
encompasses the following functionality:

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

This folder contains all of the documentation related to the runtime.

