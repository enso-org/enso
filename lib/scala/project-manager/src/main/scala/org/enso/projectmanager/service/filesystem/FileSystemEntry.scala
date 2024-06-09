package org.enso.projectmanager.service.filesystem

import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}
import org.enso.projectmanager.data.ProjectMetadata
import org.enso.projectmanager.infrastructure.file.FileJson._

import java.io.File

/** Base trait for the file system entries. */
sealed trait FileSystemEntry {

  /** A path to the file system entry. */
  def path: File

  /** An attributes of the file system entry. */
  def attributes: Attributes
}

object FileSystemEntry {

  /** A file.
    *
    * @param path the path to the file
    * @param attributes the attributes of the file
    */
  case class FileEntry(path: File, attributes: Attributes)
      extends FileSystemEntry

  /** A directory.
    *
    * @param path the path to the directory
    * @param attributes the attributes of the directory
    */
  case class DirectoryEntry(path: File, attributes: Attributes)
      extends FileSystemEntry

  /** A directory containing a project.
    *
    * @param path the path to the directory
    * @param attributes the attributes of the directory
    * @param metadata the project metadata
    */
  case class ProjectEntry(
    path: File,
    attributes: Attributes,
    metadata: ProjectMetadata
  ) extends FileSystemEntry

  private object CodecField {

    val Type = "type"

    val Path = "path"

    val Attributes = "attributes"

    val Metadata = "metadata"
  }

  private object CodecType {

    val File = "FileEntry"

    val Directory = "DirectoryEntry"

    val Project = "ProjectEntry"
  }

  implicit val encoder: Encoder[FileSystemEntry] =
    Encoder.instance[FileSystemEntry] {
      case FileEntry(path, attributes) =>
        Json.obj(
          CodecField.Type       -> CodecType.File.asJson,
          CodecField.Path       -> path.asJson,
          CodecField.Attributes -> attributes.asJson
        )
      case DirectoryEntry(path, attributes) =>
        Json.obj(
          CodecField.Type       -> CodecType.Directory.asJson,
          CodecField.Path       -> path.asJson,
          CodecField.Attributes -> attributes.asJson
        )
      case ProjectEntry(path, attributes, metadata) =>
        Json.obj(
          CodecField.Type       -> CodecType.Project.asJson,
          CodecField.Path       -> path.asJson,
          CodecField.Attributes -> attributes.asJson,
          CodecField.Metadata   -> metadata.asJson
        )
    }

  implicit val decoder: Decoder[FileSystemEntry] =
    Decoder.instance { cursor =>
      cursor.downField(CodecField.Type).as[String].flatMap {
        case CodecType.File =>
          for {
            path       <- cursor.downField(CodecField.Path).as[File]
            attributes <- cursor.downField(CodecField.Attributes).as[Attributes]
          } yield FileEntry(path, attributes)

        case CodecType.Directory =>
          for {
            path       <- cursor.downField(CodecField.Path).as[File]
            attributes <- cursor.downField(CodecField.Attributes).as[Attributes]
          } yield DirectoryEntry(path, attributes)

        case CodecType.Project =>
          for {
            path       <- cursor.downField(CodecField.Path).as[File]
            attributes <- cursor.downField(CodecField.Attributes).as[Attributes]
            metadata <- cursor
              .downField(CodecField.Metadata)
              .as[ProjectMetadata]
          } yield ProjectEntry(path, attributes, metadata)
      }
    }
}
