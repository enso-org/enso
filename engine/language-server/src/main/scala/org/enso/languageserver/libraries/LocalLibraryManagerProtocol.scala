package org.enso.languageserver.libraries

import org.enso.editions.LibraryName

object LocalLibraryManagerProtocol {
  sealed trait Request

  case class GetMetadata(libraryName: LibraryName) extends Request
  case class GetMetadataResponse(
    description: Option[String],
    tagLine: Option[String]
  )

  case class SetMetadata(
    libraryName: LibraryName,
    description: Option[String],
    tagLine: Option[String]
  ) extends Request

  case object ListLocalLibraries extends Request
  case class ListLocalLibrariesResponse(libraries: Seq[LibraryEntry])

  case class Create(
    libraryName: LibraryName,
    authors: Seq[String],
    maintainers: Seq[String],
    license: String
  ) extends Request

  case class Publish(
    libraryName: LibraryName,
    authToken: String,
    bumpVersionAfterPublish: Boolean
  ) extends Request
}
