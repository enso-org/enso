package org.enso.compiler.exception

/** A compiler internal error for "should never happen" situations.
  *
  * @param message the message for the exception
  */
class CompilerError(message: String)
    extends RuntimeException(s"Compiler Internal Error: $message")
