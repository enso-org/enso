package org.enso.editions.provider

sealed trait EditionLoadingError
case object EditionNotFound                   extends EditionLoadingError
case class EditionReadError(error: Throwable) extends EditionLoadingError
