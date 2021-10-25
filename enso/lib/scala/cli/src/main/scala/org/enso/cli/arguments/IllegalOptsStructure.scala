package org.enso.cli.arguments

/** Exception that is reported when Opts are combined in an illegal way.
  */
case class IllegalOptsStructure(message: String, cause: Throwable = null)
    extends RuntimeException(message, cause)
