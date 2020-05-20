package org.enso.languageserver.io

/**
  * A base trait for output kind.
  */
sealed trait OutputKind

object OutputKind {

  /**
    * Standard output is a stream to which a program writes its output data.
    */
  case object StandardOutput extends OutputKind

  /**
    * Standard error is output stream used by programs to write error messages
    * or status information.
    */
  case object StandardError extends OutputKind

}
