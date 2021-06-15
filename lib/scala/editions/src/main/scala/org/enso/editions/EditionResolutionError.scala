package org.enso.editions

import cats.Show

/** Indicates an error during resolution of a raw edition. */
sealed class EditionResolutionError(message: String, cause: Throwable = null)
    extends RuntimeException(message, cause)

object EditionResolutionError {

  /** Indicates that a parent edition referenced in one of the editions that are
    * being loaded cannot be loaded.
    */
  case class CannotLoadEdition(name: String, cause: Throwable)
      extends EditionResolutionError(
        s"Cannot load the edition: ${cause.getMessage}",
        cause
      )

  /** Indicates that the edition cannot be parsed. */
  case class EditionParseError(message: String, cause: Throwable)
      extends EditionResolutionError(message, cause)

  /** Indicates that a library defined in an edition references a repository
    * that is not defined in that edition or any of its parents, and so such a
    * reference is invalid.
    */
  case class LibraryReferencesUndefinedRepository(
    libraryName: String,
    repositoryName: String
  ) extends EditionResolutionError(
        s"A library `$libraryName` references a repository `$repositoryName` " +
        s"that is not defined in the edition or its parents."
      )

  /** Indicates that the chain of parent editions forms a cycle which is not
    * allowed.
    */
  case class EditionResolutionCycle(editions: List[String])
      extends EditionResolutionError(
        s"Edition resolution encountered a cycle: ${editions.mkString(" -> ")}"
      )

  /** Wraps a Circe's decoding error into a more user-friendly error. */
  def wrapDecodingError(decodingError: io.circe.Error): EditionParseError = {
    val errorMessage =
      implicitly[Show[io.circe.Error]].show(decodingError)
    EditionParseError(
      s"Could not parse the edition: $errorMessage",
      decodingError
    )
  }

  /** Wraps a general error thrown when loading a parsing an edition into a more
    * specific error type.
    */
  def wrapLoadingError(
    editionName: String,
    throwable: Throwable
  ): EditionResolutionError =
    throwable match {
      case decodingError: io.circe.Error => wrapDecodingError(decodingError)
      case other                         => CannotLoadEdition(editionName, other)
    }
}
