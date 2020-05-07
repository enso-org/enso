# Scoping
In addition to the scoping rules defined in the language semantics document,
the implementation makes some additions or alterations not visible in the
surface language that nevertheless change the scoping rules as implemented in
the runtime.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Function Call Arguments](#function-call-arguments)
- [Collapsing Scopes](#collapsing-scopes)

<!-- /MarkdownTOC -->

## Function Call Arguments
In order to support suspended function arguments in the interpreter in a
performant way, we implicitly wrap _all_ function arguments in a suspension. In
conjunction with making the function itself responsible for when its arguments
are evaluated, this lets us have incredibly performant suspended computations in
Enso.

However, it _does_ require creating a small hack in the Alias Analysis process:

- In order for an expression to be a suspension, it must occur in its own scope
  (the suspended scope).
- Alias analysis must account for this, otherwise the code generator will get
  frame accesses incorrect.

To this end, we account for this implementation detail in Alias analysis.

## Collapsing Scopes
Another quirk of the internal alias analysis process is down to the fact that
the Enso IR represents Methods, functions, and blocks as separate constructs.
This means that if you had a method containing a function containing a block, a
naive implementation of alias analysis would allocate three scopes here.

This is incorrect, according to the semantic specification of the language, so
the alias analysis process needs to handle the collapsing of these scopes as it
allocates them. The rules are as follows:

- If you have a method whose body is a function, they share a scope.
- If you have a function whose body is a block, they share a scope.
