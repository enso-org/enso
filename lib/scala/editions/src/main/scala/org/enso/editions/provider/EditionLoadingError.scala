package org.enso.editions.provider

sealed class EditionLoadingError(message: String, cause: Throwable = null)
    extends RuntimeException(message, cause)
case class EditionNotFound()
    extends EditionLoadingError("The edition was not found.")
case class EditionReadError(error: Throwable)
    extends EditionLoadingError(
      s"Could not read the edition file: ${error.getMessage}",
      error
    )
