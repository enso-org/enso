# Polyglot Syntax
Enso is a language with first-class polyglot support. In essence, this means
that Enso has first-class support for high-level interoperation with a variety
of programming languages.

It supports this through two main mechanisms:

1. **Polyglot FFI:** The low-level polyglot support provides a fairly low-level
   syntax sugar system for working with values from foreign languages.
2. **Embedded Syntax:** This system allows users to write code from other
   languages directly in their `.enso` files, and to seamlessly share values
   between Enso and that foreign code.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Polyglot FFI](#polyglot-ffi)
  - [Importing Polyglot Bindings](#importing-polyglot-bindings)
  - [Using Polyglot Bindings](#using-polyglot-bindings)
- [Embedded Syntax](#embedded-syntax)

<!-- /MarkdownTOC -->

## Polyglot FFI
The polyglot FFI is a low-level multi-language foreign function interface. It
allows users to import bindings from other languages and call them through a
generic mechanism.

### Importing Polyglot Bindings
Polyglot bindings can be imported using a polyglot import directive. This is
constructed as follows:

- The `polyglot` keyword
- A language identifier (e.g. `java`).
- The keyword `import`.
- Optionally (where the language supports it), an identifier for the type of
  language entity being imported (e.g. `struct` for `c`).
- A path that uniquely identifies the polyglot object to import.
- Optionally, the keyword `as`, followed by a new name.

For example:

```ruby
polyglot java import org.example.MyClass as MyClassJava
polyglot c import struct NetworkPacket as NetworkPacketC
```

### Using Polyglot Bindings
A polyglot binding is a polyglot object that has methods and/or fields defined
on it. Due to an impedance mismatch between languages, Enso implements a
variadic syntax for calling these polyglot bindings using vectors.

In essence, we have a primitive function as follows:

```ruby
Polyglot.method : Polyglot.Object -> [Any] -> Any
```

It works as follows:

- It is a method called `method` defined on the `Polyglot` type. The name
  `method` is, however, a stand-in for the name of the method in question.
- It takes an object instance of the polyglot object.
- It takes a vector of arguments (and is hence variadic).
- And it returns some value.

By way of example, the following is a valid usage of a polyglot binding:

```ruby
polyglot java import com.example.MyClass as MyClassJava

main =
    x = MyClassJava.foo [1, 2, 3]    # a static method
    inst = MyClassJava.new [a, b, c] # a constructor
    bar = inst.metod [x, y]          # a static method
```

## Embedded Syntax
This high-level polyglot functionality takes the form of using the syntax of
other languages _directly_ embedded in `.enso` files. A polyglot block is
introduced as follows:

- The `polyglot` keyword starts a block.
- This must be followed by a language identifier (e.g. `java`).
- After the language identifier, the remaining syntax behaves like it is an
  Enso function definition until the `=`.
- After the `=`, the user may write their foreign code.

```ruby
polyglot python concat a b =
  def concat(a, b):
    str(a) + str(b)
```

In the above example, this defines a function `concat` that takes two arguments
`a` and `b`, implemented in Python.

> The actionables for this section are:
>
> - Greatly flesh out the syntax for the high-level polyglot functionality.
