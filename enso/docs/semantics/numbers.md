---
layout: developer-doc
title: Numbers
category: semantics
tags: [semantics, runtime, number]
order: 8
---

# Numbers

In order to enhance the user experience, Enso provides a number hierarchy,
encompassing both integers of unbound size and floating-point decimals.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Number Types](#number-types)
- [Internal Representation](#internal-representation)
- [Type Conversions](#type-conversions)

<!-- /MarkdownTOC -->

## Number Types

The base number type in Enso is `Number`. It includes both integers and
floating-point numbers and is the basic type that should be used whenever
numerical computations are performed. Any method defined on the type `Number` is
automatically available on all types of numeric values.

The hierarchy is further split into the `Integer` and `Decimal` types. `Integer`
is capable of representing values of unbound length. `Decimal` is capable of
representing IEEE 64-bit (double precision) floating numbers.

Any method defined on `Integer` is available for all integers, while any method
defined on `Decimal` is available on all floating-point numbers. Methods defined
on `Integer` or `Decimal` take precedence over methods defined on `Number`, when
name resolution is performed.

## Internal Representation

Integers that can be represented in a 64-bit integer type are represented as
such. When a 64-bit representation would overflow (either by the result of
creating a large number literal or an arithmetic operation), it is represented
in a Java `BigInteger` type, thus becoming significantly slower than the 64-bit
representation.

Decimals are represented using the Java `double` type.

## Type Conversions

Number literals that do not contain a decimal point, are treated as integers.
Other literals are interpreted as decimals (even if the fractional part is 0).

Any arithmetic operation where at least one of the operands is a decimal will
result in a decimal result.

Moreover, the default division operator `/` is implemented as floating-point
division and always returns a decimal. If the desired behavior is integral
division instead, the `Integer.div` method implements it.

Another operator worth noting is the exponentiation operator (`^`). It will
always result in a decimal whenever either operand is decimal or the exponent is
negative. It will also return a float result when the exponent is outside the
64-bit integer range.

There is a `Number.to_decimal` method, that allows converting any number to a
decimal. This is useful in certain high-performance and polyglot applications.
