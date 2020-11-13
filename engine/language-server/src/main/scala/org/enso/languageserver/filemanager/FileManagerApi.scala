package org.enso.languageserver.filemanager

import org.enso.jsonrpc.{Error, HasParams, HasResult, Method, Unused}

/**
  * The file manager JSON RPC API provided by the language server.
  * See [[https://github.com/enso-org/enso/blob/main/docs/language-server/README.md]]
  * for message specifications.
  */
object FileManagerApi {

  case object WriteFile extends Method("file/write") {

    case class Params(path: Path, contents: String)

    implicit val hasParams = new HasParams[this.type] {
      type Params = WriteFile.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = Unused.type
    }
  }

  case object ReadFile extends Method("file/read") {

    case class Params(path: Path)

    case class Result(contents: String)

    implicit val hasParams = new HasParams[this.type] {
      type Params = ReadFile.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = ReadFile.Result
    }
  }

  case object CreateFile extends Method("file/create") {

    case class Params(`object`: FileSystemObject)

    implicit val hasParams = new HasParams[this.type] {
      type Params = CreateFile.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = Unused.type
    }
  }

  case object DeleteFile extends Method("file/delete") {

    case class Params(path: Path)

    implicit val hasParams = new HasParams[this.type] {
      type Params = DeleteFile.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = Unused.type
    }
  }

  case object CopyFile extends Method("file/copy") {

    case class Params(from: Path, to: Path)

    implicit val hasParams = new HasParams[this.type] {
      type Params = CopyFile.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = Unused.type
    }
  }

  case object MoveFile extends Method("file/move") {

    case class Params(from: Path, to: Path)

    implicit val hasParams = new HasParams[this.type] {
      type Params = MoveFile.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = Unused.type
    }
  }

  case object ExistsFile extends Method("file/exists") {

    case class Params(path: Path)

    case class Result(exists: Boolean)

    implicit val hasParams = new HasParams[this.type] {
      type Params = ExistsFile.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = ExistsFile.Result
    }
  }

  case object ListFile extends Method("file/list") {

    case class Params(path: Path)

    case class Result(paths: Vector[FileSystemObject])

    implicit val hasParams = new HasParams[this.type] {
      type Params = ListFile.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = ListFile.Result
    }
  }

  case object TreeFile extends Method("file/tree") {

    case class Params(path: Path, depth: Option[Int])

    case class Result(tree: DirectoryTree)

    implicit val hasParams = new HasParams[this.type] {
      type Params = TreeFile.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = TreeFile.Result
    }
  }

  case object InfoFile extends Method("file/info") {

    case class Params(path: Path)

    case class Result(attributes: FileAttributes)

    implicit val hasParams = new HasParams[this.type] {
      type Params = InfoFile.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = InfoFile.Result
    }
  }

  case object EventFile extends Method("file/event") {

    case class Params(path: Path, kind: FileEventKind)

    implicit val hasParams = new HasParams[this.type] {
      type Params = EventFile.Params
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

}
