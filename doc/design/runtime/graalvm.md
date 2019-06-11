# GraalVM
The following are notes based on questions that we have about using GraalVM to
provide a backend for Enso.

- C-API is a potentially useful tool for combining the Java/Truffle side and the
  Haskell side.
- The polyglot API is the bindings to Graal itself, with access from both C and
  Java. This is _not_ the same thing as the truffle framework for building new
  programming languages.
- You can define your own C-API (as C-entry points) for your hosted language,
  which then allows for you to call into the hosted language.
- There are no current plans to allow for compilation of the hosted language to
  native code via SubstrateVM. You can store part of the JVM heap in the native
  image, allowing for standalone executables. Serialisation of heap into the
  image.
- JavaScript and LLVM are possible backends for SubstrateVM, so we can compile
  Enso code indirectly to JS. The LLVM backend is on the roadmap, while the
  former is possible.
- No explicit control of memory layout for the hosted language, instead with a
  high level (JS-style) API for working with objects. There is no
  non-encapsulated mechanism for accessing objects due to optimisation reasons.
  If we wanted explicit memory layout control. Hybrid systems are supported.
- GraalPython is at an early stage of development, with a critical focus on the
  scientific libraries. It is very much a first-class language on the Graal
  platform.
- The Graal garbage collector is evolving to be a Java port of G1. Fairly high
  priority to improve the GC, and its support for immutability.
- Graal doesn't currently have support for saving the heap except via something
  like SubstrateVM.
- Caching: Truffle is designed for live specialisation, and has support for
  inline caching. No support for retention or eviction at this stage.
- Truffle provides some support for reusable front-end optimisations in the form
  of AST specialisation. Strong support for AST re-writing as an optimisation
  technique. You can walk the tree at creation time, or at execution time, in
  order to implement more global optimisations. Separation between allocation
  and re-writing.
- Commercial support for bugfixes, but not necessarily for feature requests yet.
- Introspection/Debugging framework lets you introspect all portions of the code
  during execution. We can dynamically add those introspection hooks in order to
  do this optimally.
- There is no way to force compilation, but could modify truffle to add some
  support for an API that allows this control. There is currently only one tier
  of compilation, but it is _possible_ to write additional JIT phases.
- There is the polyglot REPL, but it is definitely possible to build a proper
  polyglot repl based around Enso. Take a look at `polyglot - shell`.

Sit down with Chris in London with some concrete code and examples.

## A Design Based on GraalVM

- The parser remains in Haskell.
- The backend works on AST changesets instead. 
- The compiler is written in Scala using Truffle, implementing the following set
  of processes:
  + Ingestion: Transformation of AST to Graal internal AST.
  + Desugaring: Removal of all syntax sugar from the expressions to create a
    core language.
  + Typechecking: Typechecking of the core language.
  + Optimisation: Optimisation of the core language.
  + Interpretation: Using GraalVM.

Roadmap:

1. Write up a specification of the runtime, type system, and syntax.
2. WD fixes the parser for the new syntax including markers, double rep, and AST
   printing.
3. Build the first version of the GraalVM backend to operate in a standalone
   fashion, with no interactivity. This will have no standard library support.
4. Build the standard library, initially focusing on the basics, HTTP, and the
   primary interchange formats (JSON and YAML). 
5. Design the IDE protocol and implement the necessary introspection 
   functionality. The messages should only be implemented enough to support the
   Enso Studio use-cases.
6. Implement the caching in earnest.
7. Implement the typechecker and rework caching as needed. 
8. Implement the necessary sets of optimisation passes in GraalVM.
