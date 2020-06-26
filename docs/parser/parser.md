# Parser Design

## 1. Lexer (Code -> Token Stream)

> Yes. Need to understand the code first. The lexer will use flexer as a dep and
> produce a lexer in its own crate.

- Lexer needs to be generic over the input stream encoding to support utf-16
  coming from the JVM.

> Generic reader format for UTF-8 and UTF-16, using lazy reading.

- I don't think it makes sense to have separate `Var` and `Cons` identifiers. We
  should instead have `Name`, with functions `is_referrent` and `is_variable`.
  This better mirrors how the language actually treats names.

> We keep `Var` and `Cons` in the token stream, but they can be merged later in
> the AST. The purpose of the lexer is to chunk the character stream for making
> later processing fast, _and_ to distinguish blocks.

- What should the AST look like?

> Final AST and token stream AST will probably be separate entirely.

- The token stream should contain `Invalid`, yes, but what about `Ambiguous`.

> Ambiguous is only for macro resolution and cannot occur during the lexer.

- We need to be able to provide descriptive syntax errors from the _parser_.
  What we currently do in the engine is too ad-hoc to be properly feasible in
  the long term. I don't mean that the parser should _fail_, but that it should
  output descriptive error nodes in the IR. So `Invalid` and `Ambiguous` should
  contain a reason.
- Is this system going to give us the features we want around:
    + Providing specific and descriptive error nodes.
    + An AST supporting high-level language constructs

> We can definitely do this. Better errors during macro resolution.

Lexer reads source file (lazily, line by line) or uses in-memory `&str` and
produces token stream of `Var`, `Cons`, `Opr`, `Number`, `Text`, `Invalid`, and
`Block`. Please note that `Block` is part of the token stream on purpose. It is
important that the source code is easy to parse visually, so if you see a block,
it should be a block. Discovering blocks in lexer allows us to prevent all other
parts of parser, like macros, from breaking this assumption. Moreover, it makes
the design of the following stages a lot simpler.  Enso lexer should always
succeed, on any input stream (token stream could contain `Invalid` tokens).

## 2. Macro Resolution (Token Stream -> Chunked AST Stream incl spaec-unaware AST)

To be described in detail taking into consideration all current use cases. For
the current documentation of macro resolution, take a look here:
https://github.com/luna/enso/blob/main/lib/syntax/specialization/shared/src/main/scala/org/enso/syntax/text/Parser.scala

Before implementing this step, we need to talk about handling of space-unaware
AST (the AST produced by user-macros).

## 3. Operator Resolution (Chunked AST Stream -> Chunked AST Stream with Opr Apps)

Using modified [Shunting-yard algorithm](https://en.wikipedia.org/wiki/Shunting-yard_algorithm).
The algorithm is modified to support sections. The Scala implementation is
here: https://github.com/luna/enso/blob/main/lib/syntax/definition/src/main/scala/org/enso/syntax/text/prec/Operator.scala.
Unfortunately, we cannot use recursion in Rust, so it needs to be re-worked.

## 4. Finalization and Special Rules Discovery (Chunked AST Stream with Opr Apps -> AST)

To be described in detail taking into consideration all current use cases.

> How do we make macro precedence work?
