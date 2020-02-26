package org.enso.languageserver.filemanager

import org.enso.languageserver.jsonrpc.{
  Error,
  HasParams,
  HasResult,
  Method,
  Unused
}

/**
  * The file manager JSON RPC API provided by the language server.
  * See [[https://github.com/luna/enso/blob/master/doc/design/engine/engine-services.md]]
  * for message specifications.
  */
object FileManagerApi {

  case object FileWrite extends Method("file/write") {

    case class Params(path: Path, contents: String)

    implicit val hasParams = new HasParams[this.type] {
      type Params = FileWrite.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = Unused.type
    }
  }

  case object FileRead extends Method("file/read") {

    case class Params(path: Path)

    case class Result(contents: String)

    implicit val hasParams = new HasParams[this.type] {
      type Params = FileRead.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = FileRead.Result
    }
  }

  case class FileSystemError(override val message: String)
      extends Error(1000, message)

  case object ContentRootNotFoundError
      extends Error(1001, "Content root not found")

  case object AccessDeniedError extends Error(1002, "Access denied")

  case object FileNotFoundError extends Error(1003, "File not found")

}
