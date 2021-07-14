package org.enso.languageserver.libraries

import org.enso.editions.LibraryName

object LocalLibraryManagerProtocol {
  sealed trait Request

  case class GetMetadata(libraryName: LibraryName) extends Request
  case class GetMetadataResponse(
    description: Option[String],
    tagLine: Option[String]
  )

  case object Success
  case class SetMetadata(
    libraryName: LibraryName,
    description: Option[String],
    tagLine: Option[String]
  ) extends Request

  case object List extends Request
  case class ListResponse(libraries: Seq[LibraryName])

  case class Publish(
    libraryName: LibraryName,
    authToken: String,
    bumpVersionAfterPublish: Boolean
  ) extends Request
}
