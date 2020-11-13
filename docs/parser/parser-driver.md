---
layout: developer-doc
title: Parser Driver
category: parser
tags: [parser, driver]
order: 8
---

# Parser Driver

The parser driver component is responsible for orchestrating the entire action
of the parser. It handles the following duties:

1.  Consuming input text using a provided [reader](./reader.md) in a lazy
    fashion.
2.  Lexing and then parsing the input text.
3.  Writing the output AST to the client of the parser.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Driver Clients](#driver-clients)

<!-- /MarkdownTOC -->

## Driver Clients

The parser is going to be employed in two contexts, both running in-process:

1. In the IDE codebase as a rust dependency.
2. In the engine as a native code dependency used via JNI.
