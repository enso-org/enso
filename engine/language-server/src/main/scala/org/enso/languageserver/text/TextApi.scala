package org.enso.languageserver.text

import org.enso.languageserver.data.CapabilityRegistration
import org.enso.languageserver.filemanager.Path
import org.enso.jsonrpc.{Error, HasParams, HasResult, Method, Unused}

/**
  * The text editing JSON RPC API provided by the language server.
  * See [[https://github.com/luna/enso/blob/main/docs/language-server/README.md]]
  * for message specifications.
  */
object TextApi {

  case object OpenFile extends Method("text/openFile") {
    case class Params(path: Path)
    case class Result(
      writeCapability: Option[CapabilityRegistration],
      content: String,
      currentVersion: String
    )
    implicit val hasParams = new HasParams[this.type] {
      type Params = OpenFile.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = OpenFile.Result
    }
  }

  case object CloseFile extends Method("text/closeFile") {
    case class Params(path: Path)
    implicit val hasParams = new HasParams[this.type] {
      type Params = CloseFile.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = Unused.type
    }
  }

  case object ApplyEdit extends Method("text/applyEdit") {
    case class Params(edit: FileEdit)
    implicit val hasParams = new HasParams[this.type] {
      type Params = ApplyEdit.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = Unused.type
    }
  }

  case object TextDidChange extends Method("text/didChange") {
    case class Params(edits: List[FileEdit])
    implicit val hasParams = new HasParams[this.type] {
      type Params = TextDidChange.Params
    }
  }

  case object FileNotOpenedError extends Error(3001, "File not opened")

  case class TextEditValidationError(msg: String) extends Error(3002, msg)
  case class InvalidVersionError(
    clientVersion: Buffer.Version,
    serverVersion: Buffer.Version
  ) extends Error(
        3003,
        s"Invalid version [client version: $clientVersion, server version: $serverVersion]"
      )
  case object WriteDeniedError extends Error(3004, "Write denied")

  case object SaveFile extends Method("text/save") {
    case class Params(path: Path, currentVersion: Buffer.Version)
    implicit val hasParams = new HasParams[this.type] {
      type Params = SaveFile.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = Unused.type
    }
  }

}
