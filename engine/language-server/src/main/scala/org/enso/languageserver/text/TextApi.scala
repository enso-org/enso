package org.enso.languageserver.text

import org.enso.languageserver.data.CapabilityRegistration
import org.enso.languageserver.filemanager.Path
import org.enso.jsonrpc.{Error, HasParams, HasResult, Method, Unused}
import org.enso.polyglot.runtime.Runtime.Api.ExpressionId
import org.enso.text.editing.model.TextEdit

/** The text editing JSON RPC API provided by the language server.
  * See [[https://github.com/enso-org/enso/blob/develop/docs/language-server/README.md]]
  * for message specifications.
  */
object TextApi {

  type Version = String

  case object OpenFile extends Method("text/openFile") {
    case class Params(path: Path)
    case class Result(
      writeCapability: Option[CapabilityRegistration],
      content: String,
      currentVersion: Version
    )
    implicit val hasParams = new HasParams[this.type] {
      type Params = OpenFile.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = OpenFile.Result
    }
  }

  case object OpenBuffer extends Method("text/openBuffer") {
    case class Params(path: Path)
    case class Result(
      writeCapability: Option[CapabilityRegistration],
      content: String,
      currentVersion: Version
    )
    implicit val hasParams = new HasParams[this.type] {
      type Params = OpenBuffer.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = OpenBuffer.Result
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
    case class Params(edit: FileEdit, execute: Option[Boolean])
    implicit val hasParams = new HasParams[this.type] {
      type Params = ApplyEdit.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = Unused.type
    }
  }

  case object ApplyExpressionValue extends Method("text/applyExpressionValue") {
    case class Params(
      expressionId: ExpressionId,
      path: Path,
      edit: TextEdit,
      oldVersion: TextApi.Version,
      newVersion: TextApi.Version
    )
    implicit val hasParams = new HasParams[this.type] {
      type Params = ApplyExpressionValue.Params
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
    clientVersion: Version,
    serverVersion: Version
  ) extends Error(
        3003,
        s"Invalid version [client version: $clientVersion, server version: $serverVersion]"
      )
  case object WriteDeniedError extends Error(3004, "Write denied")

  case object SaveFile extends Method("text/save") {
    case class Params(path: Path, currentVersion: Version)
    implicit val hasParams = new HasParams[this.type] {
      type Params = SaveFile.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = Unused.type
    }
  }

}
