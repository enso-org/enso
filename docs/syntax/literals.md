---
layout: developer-doc
title: Literals
category: syntax
tags: [syntax, literals]
order: 4
---

# Literals
Enso supports a small set of literals that allow the expression of some common
types in literal form in the source code.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Numeric Literals](#numeric-literals)
- [Text Literals](#text-literals)
  - [Inline Text Literals](#inline-text-literals)
  - [Text Block Literals](#text-block-literals)
- [Vector Literals](#vector-literals)

<!-- /MarkdownTOC -->

## Numeric Literals
Enso provides rich support for numeric literals, including literals that use
different numeric bases. It does, of course, support floating point numerals as
well.

A numeric literal takes the form:

```ebnf
digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
hex = "a" | "b" | "c" | "d" | "e" | "f";
number-digit = digit | hex;
decimal-point = ".";

float-digit = number-digit | decimal-point;

base-specifier = { digit };

numeric-literal = [base-specifier, "_"], { number-digit };
```

If no base is specified, it is inferred to be a standard base-10 numeral.

Some examples of numeric literals follow:

```ruby
decimal          = 12345.39
decimal_explicit = 10_1029301
octal            = 8_122137
hex              = 16_ae2f14
binary           = 2_10011101010
```

> Actionables for this section are:
>
> - Think about whether we want to support explicit fractional and complex
>   literals, or whether these should be relegated to type constructors.

## Text Literals
Enso provides rich support for textual literals in the language, supporting both
raw and interpolated strings natively.

- **Raw Strings:** Raw strings are delimited using the standard double-quote
  character (`"`). Raw strings have support for escape sequences.

  ```ruby
  raw_string = "Hello, world!"
  ```

- **Interpolated Strings:** Interpolated strings support the splicing of
  executable Enso expressions into the string. Such strings are delimited using
  the single-quote (`'`) character, and splices are delimited using the backtick
  (`` ` ``) character. Splices are run, and then the result is converted to a
  string using `show`. These strings also have support for escape sequences.

  ```ruby
  fmt_string = 'Hello, my age is `time.now.year - person.birthday.year`'
  ```

### Inline Text Literals
In Enso, inline text literals are opened and closed using the corresponding
quote type for the literal. They may contain escape sequences but may _not_ be
broken across lines.

```ruby
inline_raw = "Foo bar baz"
inline_interpolated = 'Foo `bar` baz'
```

### Text Block Literals
In Enso, text block literals rely on _layout_ to determine the end of the block,
allowing users to only _open_ the literal. Block literals are opened with three
of the relevant quote type, and the contents of the block are determined by the
following layout rules:

- The first child line of the block sets the baseline left margin for the block.
  Any indentation up to this margin will be removed.
- Any indentation further than this baseline will be retained as part of the
  text literal.
- The literal is _closed_ by the first line with a _lower_ level of indentation
  than the first child lineand will not contain the final blank line.

```
block_raw = '''
    part of the string
        still part of the string

    also part of the string

not_string_expr = foo bar
```

## Vector Literals
Enso also supports vector literals, which allow users to create literal vectors
of elements.

```ruby
literal = [elem_1, elem_2, elem_3, ...]
```

A vector literal works as follows:

- It is begun by the `[` character.
- It is ended by the `]` character.
- Elements in vector literals are concatenated using the `,` operator, which
  acts as `cons` on vectors.
