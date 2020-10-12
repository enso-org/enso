---
layout: developer-doc
title: Reading Source Code
category: parser
tags: [parser, reader]
order: 11
---

# Reading Source Code

The reader is responsible for abstracting the interface to reading a character
from a stream. This handles abstracting away the various encodings that the
project is going to use, as well as backing formats for the stream.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Reader Functionality](#reader-functionality)
- [Provided Readers](#provided-readers)
  - [UTF-8 Reader](#utf-8-reader)
  - [UTF-16 Reader](#utf-16-reader)

<!-- /MarkdownTOC -->

## Reader Functionality

The reader trait needs to have the following functionality:

- It must read its input _lazily_, not requiring the entire input to be in
  memory.
- It should provide the interface to `next_character`, returning rust-native
  UTF-8, and hence abstract away the various underlying encodings.

## Provided Readers

The parser implementation currently provides the following reader utilities to
clients.

### UTF-8 Reader

Rust natively uses UTF-8 encoding for its strings. In order for the IDE to make
use of the parser, it must provide a simple rust-native reader.

### UTF-16 Reader

As the JVM as a platform makes use of UTF-16 for encoding its strings, we need
to provide a reader that will let JVM clients of the parser provide the source
code in a streaming fashion without needing to re-encode it prior to passing it
to the parser.
