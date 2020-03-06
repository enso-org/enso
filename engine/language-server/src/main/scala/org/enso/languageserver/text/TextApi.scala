package org.enso.languageserver.text

import org.enso.languageserver.data.CapabilityRegistration
import org.enso.languageserver.filemanager.Path
import org.enso.languageserver.jsonrpc.{
  Error,
  HasParams,
  HasResult,
  Method,
  Unused
}

/**
  * The text editing JSON RPC API provided by the language server.
  * See [[https://github.com/luna/enso/blob/master/doc/design/engine/engine-services.md]]
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

  case object FileNotOpenedError extends Error(3001, "File not opened")

}
