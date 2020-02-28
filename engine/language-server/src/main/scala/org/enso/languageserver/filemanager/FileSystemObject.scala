package org.enso.languageserver.filemanager

import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}

/**
  * A representation of filesystem object.
  */
sealed trait FileSystemObject

object FileSystemObject {

  /**
    * Represents a directory.
    *
    * @param name a name of the directory
    * @param path a path to the directory
    */
  case class Directory(name: String, path: Path) extends FileSystemObject

  /**
    * Represents a file.
    *
    * @param name a name of the file
    * @param path a path to the file
    */
  case class File(name: String, path: Path) extends FileSystemObject

  private val TypeField = "type"

  private val NameField = "name"

  private val PathField = "path"

  private val FileType = "File"

  private val DirectoryType = "Directory"

  implicit val fsoDecoder: Decoder[FileSystemObject] =
    Decoder.instance { cursor =>
      cursor.downField(TypeField).as[String].flatMap {
        case FileType =>
          for {
            name <- cursor.downField(NameField).as[String]
            path <- cursor.downField(PathField).as[Path]
          } yield File(name, path)

        case DirectoryType =>
          for {
            name <- cursor.downField(NameField).as[String]
            path <- cursor.downField(PathField).as[Path]
          } yield Directory(name, path)
      }
    }

  implicit val fsoEncoder: Encoder[FileSystemObject] =
    Encoder.instance[FileSystemObject] {
      case Directory(name, path) =>
        Json.obj(
          TypeField -> DirectoryType.asJson,
          NameField -> name.asJson,
          PathField -> path.asJson
        )

      case File(name, path) =>
        Json.obj(
          TypeField -> FileType.asJson,
          NameField -> name.asJson,
          PathField -> path.asJson
        )
    }

}
