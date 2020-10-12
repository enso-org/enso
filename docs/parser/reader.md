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

The reader has the following functionality:

- It reads its input _lazily_, not requiring the entire input to be in memory.
- It provides the interface to `next_character`, returning rust-native UTF-32,
  and abstracts away the various underlying encodings.
- It allows to bookmark the character that was last read, and return to it later
  by calling `rewind`.

## Reader Structure

The lazy reader consists of the following parts:

### Read

The `Read` trait is similar to `std::io::Read`, but supports different encodings
than just `&[u8]`. It provides the interface
`fn read(&mut self, buffer:&mut [Self::Item]) -> usize` that fills the provided
buffer with the data that is being read.

Any structure that implements `std::io::Read` also implements `Read<Item=u8>`.

### Decoder

The `Decoder` trait is an interface for reading a single character from an
underlying buffer `fn decode(words:&[Self::Word]) -> Char`. The type of buffer
depends on the type of the underlying encoding so that i.e. UTF-32 can use
`&[char]` directly.

#### Example Usage

To put things into perspective, this is how the reader is constructed from a
file and a string.

```rust
let string      = "Hello, World!";
let byte_reader = Reader::new(string.as_bytes(), DecoderUTF8(), 0);
let file_reader = Reader::new(File::open("foo.txt")?, DecoderUTF8(), 0);
```

## Provided Encodings

The decoders currently provides the following input encodings.

### UTF-8

Rust natively uses UTF-8 encoding for its strings. In order for the IDE to make
use of the parser, a simple rust-native UTF-8 encoding is provided.

### UTF-16

As the JVM as a platform makes use of UTF-16 for encoding its strings, we need
to have a reader that lets JVM clients of the parser provide the source code in
a streaming fashion without needing to re-encode it prior to passing it to the
parser.

### UTF-32

Rust also uses UTF-32 encoding for its characters. Therefore, this encoding is
required in order to support inputs as `&[char]`.

### Benchmarks

7/17/2020: The reader throughput is around 1e+8 chars/s (or 1e-8 secs/char).
