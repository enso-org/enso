package org.enso.languageserver.session

import java.util.UUID

import org.enso.jsonrpc.{Error, HasParams, HasResult, Method}

/**
  * The connection management JSON RPC API provided by the language server.
  * See [[https://github.com/luna/enso/blob/main/docs/language-server/protocol-language-server.md#connection-management]]
  * for message specifications.
  */
object SessionApi {

  case object InitProtocolConnection
      extends Method("session/initProtocolConnection") {

    case class Params(clientId: UUID)

    case class Result(contentRoots: Set[UUID])

    implicit val hasParams = new HasParams[this.type] {
      type Params = InitProtocolConnection.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = InitProtocolConnection.Result
    }
  }

  case object SessionNotInitialisedError
      extends Error(6001, "Session not initialised")

  case object SessionAlreadyInitialisedError
      extends Error(6002, "Session already initialised")

}
