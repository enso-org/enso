package org.enso.yaml

import cats.Show

/** Indicates a parse failure, usually meaning that the input data has
  * unexpected format (like missing fields or wrong field types).
  */
case class ParseError(message: String, cause: io.circe.Error)
    extends RuntimeException(message, cause)

object ParseError {

  /** Wraps a [[io.circe.Error]] into a more user-friendly [[ParseError]]. */
  def apply(error: io.circe.Error): ParseError = {
    val errorMessage =
      implicitly[Show[io.circe.Error]].show(error)
    ParseError(errorMessage, error)
  }
}
