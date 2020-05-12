# Polyglot Java Runtime
This document deals with the implementation of polyglot interoperation with
Java in the runtime.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Class Lookup](#class-lookup)
- [Polyglot Library System](#polyglot-library-system)
- [Polyglot Syntax System](#polyglot-syntax-system)

<!-- /MarkdownTOC -->

## Class Lookup
In order for the Enso runtime to effectively find Java objects for working with
in a polyglot fashion, it will look in the `polyglot/java` subdirectory of an
Enso project. This directory has the following requirements placed on it.

- The top level of the `java` directory should contain only `.jar` files and
  directories.
- Each directory must provide a valid class-path structure, with `.class` files
  at the appropriate points.
- Both `.jar` files and directories are added to the runtime class-path for
  Enso, and hence be made available to Enso programs.

> The actionables for this section are:
>
> - In future, we want to expand this to support `.class` files directly, and
>   maybe even compiling Java code.

## Polyglot Library System
The dynamic polyglot system is a dynamic runtime lookup for Java objects,
allowing Enso code to work with them through a runtime reflection-style
mechanism. It is comprised of the following components:

- `Java.lookup_class : Class.Path -> Maybe Class`: A function that lets
  users look up a class by a given name on the runtime classpath.
- `Polyglot.instantiate : Class -> Object`: A function that lets users
  instantiate a class into an object.
- A whole host of functions on the polyglot type that let you dynamically work
  with object bindings.

An example can be found below:

```ruby
main =
    class = Java.lookup_class "org.enso.example.TestClass"
    instance = Polyglot.instantiate1 class (x -> x * 2)
    method = Polyglot.get_member instance "callFunctionAndIncrement"
    Polyglot.execute1 method 10
```

> The actionables for this section are:
>
> - Expand on the detail when there is time.

## Polyglot Syntax System
The static system, however, lets us do much better in terms of user experience.
Instead of having to dynamically look things up at runtime, we can instead do
the following:

- Statically resolve imports of polyglot bindings within the project to make
  sure that they are available.
- Create java-compatible object entities that dynamically look up and dispatch
  both static methods on classes (by name), and methods on objects (by name).
  This includes the constructor.
- This invocation syntax is integrated into Enso as variadic methods, allowing
  us to deal with the inter-language impedance mismatch.

An example can be found below:

```ruby
polyglot java import com.example.MyClass as MyClassJava

main =
    x = MyClassJava.foo [1, 2, 3]
    inst = MyClassJava.new [a, b, c]
    bar = inst.methodName [x, y]
```

> The actionables for this section are:
>
> - Expand on the detail as the implementation becomes clear.
