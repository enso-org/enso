package org.enso.languageserver.workspace

import org.enso.jsonrpc.{HasParams, HasResult, Method}

/** The workspace management JSON RPC API provided by the language server.
  *
  * See [[https://github.com/enso-org/enso/blob/develop/docs/language-server/README.md]]
  * for message specifications.
  */
object WorkspaceApi {

  case object ProjectInfo extends Method("workspace/projectInfo") {
    case class Params()
    case class Result(
      projectName: String,
      engineVersion: String,
      graalVersion: String
    )
    implicit val hasParams = new HasParams[this.type] {
      type Params = ProjectInfo.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = ProjectInfo.Result
    }
  }
}
