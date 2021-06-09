package org.enso.editions

sealed class EditionResolutionError(message: String, cause: Throwable = null)
    extends RuntimeException(message, cause)

object EditionResolutionError {
  case class CannotLoadParentEdition(cause: Throwable)
      extends EditionResolutionError(
        s"Cannot load the parent edition: ${cause.getMessage}",
        cause
      )

  case class LibraryReferencesUndefinedRepository(
    libraryName: String,
    repositoryName: String
  ) extends EditionResolutionError(
        s"A library `$libraryName` references a repository `$repositoryName` " +
        s"that is not defined in the edition or its parents."
      )
}
