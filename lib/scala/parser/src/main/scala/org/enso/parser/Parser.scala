package org.enso.parser

import org.enso.ast.Ast

import scala.annotation.unused


/** This is the Enso language parser.
  *
  * It is a wrapper of parser written in Rust that uses JNI to efficiently
  * construct scala AST directly without any serialization overhead.
  *
  * The methods are loaded from a native shared library `parser` that is located
  * in a directory specified by `-Djava.library.path` and has one of the extensions
  * `.dll`, `.so` or `dylib` depending on the platform (windows, linux or mac).
  *
  * The shared library itself is generated into `target/rust/debug` by executing
  * `cargo build -p parser`. Each method marked by `@native` must have a
  * corresponding counterpart in rust, otherwise the loading of the shared library
  * is going to fail at runtime with `UnsatisfiedLinkingError`.
  */
class Parser private () {
  /**  Parses a content of a single source file. */
  @native def parseStr(@unused input: String): Ast.AnyAst

  /**  Parses a single source file. */
  @native def parseFile(@unused filename: String): Ast.AnyAst

  /**  Parses a content of a single source file into a stream of tokens. */
  @native def lexStr(@unused input: String): Ast.AnyAst

  /**  Parses a single source file into a stream of tokens. */
  @native def lexFile(@unused filename: String): Ast.AnyAst
}

object Parser {
  System.loadLibrary("parser")

  /** Constructs a new parser */
  def apply(): Parser = new Parser()
}
