---
layout: developer-doc
title: Parser Architecture Overview
category: parser
tags: [parser, architecture]
order: 2
---

# Parser Architecture Overview
The Enso parser is designed in a highly modular fashion, with separate crates
responsible for the component's various responsibilities. The main components of
the parser are described below.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Overall Architecture](#overall-architecture)
- [Reader](#reader)
- [Flexer](#flexer)
- [Lexer](#lexer)
- [Macro Resolution](#macro-resolution)
- [Operator Resolution](#operator-resolution)
- [Construct Resolution](#construct-resolution)
- [Parser Driver](#parser-driver)
  - [AST](#ast)
- [JVM Object Generation](#jvm-object-generation)

<!-- /MarkdownTOC -->

## Overall Architecture
The overall architecture of the parser subsystem can be visualised as follows.

## Reader

## Flexer

## Lexer

## Macro Resolution

## Operator Resolution

## Construct Resolution

## Parser Driver

### AST

## JVM Object Generation

- Should wrap the parser as a whole into a new module, built for the engine
