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
}

object FileSystemEntry {

  /** A file.
    *
    * @param path the path to the file
    */
  case class FileEntry(path: File) extends FileSystemEntry

  /** A directory.
    *
    * @param path the path to the directory
    */
  case class DirectoryEntry(path: File) extends FileSystemEntry

  /** A directory containing a project.
    *
    * @param path the path to the directory
    * @param metadata the project metadata
    */
  case class ProjectEntry(path: File, metadata: ProjectMetadata)
      extends FileSystemEntry

  private object CodecField {

    val Type = "type"

    val Path = "path"

    val Metadata = "metadata"
  }

  private object CodecType {

    val File = "FileEntry"

    val Directory = "DirectoryEntry"

    val Project = "ProjectEntry"
  }

  implicit val encoder: Encoder[FileSystemEntry] =
    Encoder.instance[FileSystemEntry] {
      case FileEntry(path) =>
        Json.obj(
          CodecField.Type -> CodecType.File.asJson,
          CodecField.Path -> path.asJson
        )
      case DirectoryEntry(path) =>
        Json.obj(
          CodecField.Type -> CodecType.Directory.asJson,
          CodecField.Path -> path.asJson
        )
      case ProjectEntry(path, metadata) =>
        Json.obj(
          CodecField.Type     -> CodecType.Project.asJson,
          CodecField.Path     -> path.asJson,
          CodecField.Metadata -> metadata.asJson
        )
    }

  implicit val decoder: Decoder[FileSystemEntry] =
    Decoder.instance { cursor =>
      cursor.downField(CodecField.Type).as[String].flatMap {
        case CodecType.File =>
          for {
            path <- cursor.downField(CodecField.Path).as[File]
          } yield FileEntry(path)

        case CodecType.Directory =>
          for {
            path <- cursor.downField(CodecField.Path).as[File]
          } yield DirectoryEntry(path)

        case CodecType.Project =>
          for {
            path <- cursor.downField(CodecField.Path).as[File]
            metadata <- cursor
              .downField(CodecField.Metadata)
              .as[ProjectMetadata]
          } yield ProjectEntry(path, metadata)
      }
    }
}
