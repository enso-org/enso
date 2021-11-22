package org.enso.languageserver.monitoring

import org.enso.jsonrpc.{HasParams, HasResult, Method, Unused}

/** The monitoring JSON RPC API provided by the language server.
  * See [[https://github.com/enso-org/enso/blob/develop/docs/language-server/README.md]]
  * for message specifications.
  */
object MonitoringApi {

  case object Ping extends Method("heartbeat/ping") {
    implicit val hasParams = new HasParams[this.type] {
      type Params = Unused.type
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = Unused.type
    }
  }

  case object InitialPing extends Method("heartbeat/init") {
    implicit val hasParams = new HasParams[this.type] {
      type Params = Unused.type
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = Unused.type
    }
  }

}
