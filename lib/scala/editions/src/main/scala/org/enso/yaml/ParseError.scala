package org.enso.yaml

/** Indicates a parse failure, usually meaning that the input data has
  * unexpected format (like missing fields or wrong field types).
  */
case class ParseError(message: String, cause: Throwable)
    extends RuntimeException(message, cause)

object ParseError {

  /** Wraps a parser exception into a more user-friendly [[ParseError]]. */
  def apply(error: Throwable): ParseError = {
    ParseError(error.getMessage, error)
  }
}
