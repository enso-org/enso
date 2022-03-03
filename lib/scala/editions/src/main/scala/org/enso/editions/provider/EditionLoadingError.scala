package org.enso.editions.provider

/** Indicates a failure to load an edition. */
sealed class EditionLoadingError(message: String, cause: Throwable = null)
    extends RuntimeException(message, cause)

/** Indicates that the requested edition was not found in the caches. */
case class EditionNotFound(editionName: String)
    extends EditionLoadingError("The edition was not found.")

/** Indicates that the edition was found but could not be read, for example due
  * to a filesystem error when opening the file or a parse error.
  */
case class EditionReadError(error: Throwable)
    extends EditionLoadingError(
      s"Could not read the edition file: ${error.getMessage}",
      error
    )
