package org.enso.languageserver.libraries

import io.circe.generic.semiauto._
import io.circe.syntax._
import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import org.enso.editions

case class LibraryEntry(
  namespace: String,
  name: String,
  version: LibraryEntry.LibraryVersion
)

object LibraryEntry {
  sealed trait LibraryVersion
  case object LocalLibraryVersion extends LibraryVersion
  case class PublishedLibraryVersion(version: String, repositoryUrl: String)
      extends LibraryVersion

  implicit def convertLibraryVersion(
    libraryVersion: editions.LibraryVersion
  ): LibraryVersion = libraryVersion match {
    case editions.LibraryVersion.Local => LocalLibraryVersion
    case editions.LibraryVersion.Published(version, repository) =>
      PublishedLibraryVersion(version.toString, repository.url)
  }

  implicit val encoder: Encoder[LibraryEntry] = deriveEncoder[LibraryEntry]
  implicit val decoder: Decoder[LibraryEntry] = deriveDecoder[LibraryEntry]

  object CodecField {
    val Type          = "type"
    val Version       = "version"
    val RepositoryUrl = "repositoryUrl"
  }

  object CodecType {
    val LocalLibraryVersion     = "LocalLibraryVersion"
    val PublishedLibraryVersion = "PublishedLibraryVersion"
  }

  implicit val versionEncoder: Encoder[LibraryVersion] = {
    case LocalLibraryVersion =>
      Json.obj(CodecField.Type -> CodecType.LocalLibraryVersion.asJson)
    case PublishedLibraryVersion(version, repositoryUrl) =>
      Json.obj(
        CodecField.Type          -> CodecType.PublishedLibraryVersion.asJson,
        CodecField.Version       -> version.asJson,
        CodecField.RepositoryUrl -> repositoryUrl.asJson
      )
  }

  implicit val versionDecoder: Decoder[LibraryVersion] = { json =>
    val typeCursor = json.downField(CodecField.Type)
    typeCursor.as[String].flatMap {
      case CodecType.LocalLibraryVersion =>
        Right(LocalLibraryVersion)
      case CodecType.PublishedLibraryVersion =>
        for {
          version       <- json.get[String](CodecField.Version)
          repositoryUrl <- json.get[String](CodecField.RepositoryUrl)
        } yield PublishedLibraryVersion(version, repositoryUrl)
      case unknownType =>
        Left(
          DecodingFailure(
            s"Unknown LibraryVersion type [$unknownType].",
            typeCursor.history
          )
        )
    }
  }
}
