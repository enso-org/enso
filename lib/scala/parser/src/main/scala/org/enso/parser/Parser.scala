package org.enso.parser

import org.enso.ast.Ast

import scala.annotation.nowarn


/** This is the Enso language parser.
  *
  * It is a wrapper of parser written in Rust that uses JNI to efficiently
  * construct scala AST directly without any serialization overhead.
  */
class Parser private () {
  /**  Parses a single sourcef file. */
  @native @nowarn def parse(input: String): Ast.AnyAst
}

object Parser {
  /** Constructs a new parser */
  def apply(): Parser = {
    System.loadLibrary("parser")
    new Parser()
  }
}
