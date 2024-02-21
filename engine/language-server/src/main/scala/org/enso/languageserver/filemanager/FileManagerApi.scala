package org.enso.languageserver.filemanager

import io.circe.Json
import io.circe.literal.JsonStringContext
import org.enso.jsonrpc.{Error, HasParams, HasResult, Method, Unused}

import java.util.UUID

/** The file manager JSON RPC API provided by the language server.
  * See [[https://github.com/enso-org/enso/blob/develop/docs/language-server/README.md]]
  * for message specifications.
  */
object FileManagerApi {

  case object WriteFile extends Method("file/write") {

    case class Params(path: Path, contents: String)

    implicit val hasParams: HasParams.Aux[this.type, WriteFile.Params] =
      new HasParams[this.type] {
        type Params = WriteFile.Params
      }

    implicit val hasResult: HasResult.Aux[this.type, Unused.type] =
      new HasResult[this.type] {
        type Result = Unused.type
      }
  }

  case object ReadFile extends Method("file/read") {

    case class Params(path: Path)

    case class Result(contents: String)

    implicit val hasParams: HasParams.Aux[this.type, ReadFile.Params] =
      new HasParams[this.type] {
        type Params = ReadFile.Params
      }
    implicit val hasResult: HasResult.Aux[this.type, ReadFile.Result] =
      new HasResult[this.type] {
        type Result = ReadFile.Result
      }
  }

  case object CreateFile extends Method("file/create") {

    case class Params(`object`: FileSystemObject)

    implicit val hasParams: HasParams.Aux[this.type, CreateFile.Params] =
      new HasParams[this.type] {
        type Params = CreateFile.Params
      }

    implicit val hasResult: HasResult.Aux[this.type, Unused.type] =
      new HasResult[this.type] {
        type Result = Unused.type
      }
  }

  case object DeleteFile extends Method("file/delete") {

    case class Params(path: Path)

    implicit val hasParams: HasParams.Aux[this.type, DeleteFile.Params] =
      new HasParams[this.type] {
        type Params = DeleteFile.Params
      }

    implicit val hasResult: HasResult.Aux[this.type, Unused.type] =
      new HasResult[this.type] {
        type Result = Unused.type
      }
  }

  case object CopyFile extends Method("file/copy") {

    case class Params(from: Path, to: Path)

    implicit val hasParams: HasParams.Aux[this.type, CopyFile.Params] =
      new HasParams[this.type] {
        type Params = CopyFile.Params
      }

    implicit val hasResult: HasResult.Aux[this.type, Unused.type] =
      new HasResult[this.type] {
        type Result = Unused.type
      }
  }

  case object MoveFile extends Method("file/move") {

    case class Params(from: Path, to: Path)

    implicit val hasParams: HasParams.Aux[this.type, MoveFile.Params] =
      new HasParams[this.type] {
        type Params = MoveFile.Params
      }

    implicit val hasResult: HasResult.Aux[this.type, Unused.type] =
      new HasResult[this.type] {
        type Result = Unused.type
      }
  }

  case object ExistsFile extends Method("file/exists") {

    case class Params(path: Path)

    case class Result(exists: Boolean)

    implicit val hasParams: HasParams.Aux[this.type, ExistsFile.Params] =
      new HasParams[this.type] {
        type Params = ExistsFile.Params
      }
    implicit val hasResult: HasResult.Aux[this.type, ExistsFile.Result] =
      new HasResult[this.type] {
        type Result = ExistsFile.Result
      }
  }

  case object ListFile extends Method("file/list") {

    case class Params(path: Path)

    case class Result(paths: Vector[FileSystemObject])

    implicit val hasParams: HasParams.Aux[this.type, ListFile.Params] =
      new HasParams[this.type] {
        type Params = ListFile.Params
      }
    implicit val hasResult: HasResult.Aux[this.type, ListFile.Result] =
      new HasResult[this.type] {
        type Result = ListFile.Result
      }
  }

  case object TreeFile extends Method("file/tree") {

    case class Params(path: Path, depth: Option[Int])

    case class Result(tree: DirectoryTree)

    implicit val hasParams: HasParams.Aux[this.type, TreeFile.Params] =
      new HasParams[this.type] {
        type Params = TreeFile.Params
      }
    implicit val hasResult: HasResult.Aux[this.type, TreeFile.Result] =
      new HasResult[this.type] {
        type Result = TreeFile.Result
      }
  }

  case object InfoFile extends Method("file/info") {

    case class Params(path: Path)

    case class Result(attributes: FileAttributes)

    implicit val hasParams: HasParams.Aux[this.type, InfoFile.Params] =
      new HasParams[this.type] {
        type Params = InfoFile.Params
      }
    implicit val hasResult: HasResult.Aux[this.type, InfoFile.Result] =
      new HasResult[this.type] {
        type Result = InfoFile.Result
      }
  }

  case object ChecksumFile extends Method("file/checksum") {
    case class Params(path: Path)
    case class Result(checksum: String)

    implicit val hasParams: HasParams.Aux[this.type, ChecksumFile.Params] =
      new HasParams[this.type] {
        type Params = ChecksumFile.Params
      }
    implicit val hasResult: HasResult.Aux[this.type, ChecksumFile.Result] =
      new HasResult[this.type] {
        type Result = ChecksumFile.Result
      }
  }

  case object EventFile extends Method("file/event") {

    case class Params(
      path: Path,
      kind: FileEventKind,
      attributes: Option[FileAttributes]
    )

    implicit val hasParams: HasParams.Aux[this.type, EventFile.Params] =
      new HasParams[this.type] {
        type Params = EventFile.Params
      }
  }

  case object ContentRootAdded extends Method("file/rootAdded") {
    case class Params(root: ContentRoot)

    implicit val hasParams: HasParams.Aux[this.type, ContentRootAdded.Params] =
      new HasParams[this.type] {
        type Params = ContentRootAdded.Params
      }
  }

  case object ContentRootRemoved extends Method("file/rootRemoved") {
    case class Params(id: UUID)

    implicit
    val hasParams: HasParams.Aux[this.type, ContentRootRemoved.Params] =
      new HasParams[this.type] {
        type Params = ContentRootRemoved.Params
      }
  }

  // Errors

  case class FileSystemError(override val message: String)
      extends Error(1000, message)

  case object ContentRootNotFoundError
      extends Error(1001, "Content root not found")

  case object FileNotFoundError extends Error(1003, "File not found")

  case object FileExistsError extends Error(1004, "File already exists")

  case object OperationTimeoutError extends Error(1005, "IO operation timeout")

  case object NotDirectoryError extends Error(1006, "Path is not a directory")

  case object NotFileError extends Error(1007, "Path is not a file")

  case object CannotOverwriteError
      extends Error(
        1008,
        "Cannot overwrite the file without `overwriteExisting` set"
      )

  case class ReadOutOfBoundsError(length: Long)
      extends Error(1009, "Read is out of bounds for the file") {
    override def payload: Option[Json] = Some(
      json""" { "fileLength" : $length }"""
    )
  }

  case object CannotDecodeError
      extends Error(1010, "Cannot decode the project configuration")

}
