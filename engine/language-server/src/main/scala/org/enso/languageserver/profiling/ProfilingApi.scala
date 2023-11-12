package org.enso.languageserver.profiling

import org.enso.jsonrpc.{HasParams, HasResult, Method, Unused}

object ProfilingApi {

  case object ProfilingStart extends Method("profiling/start") {

    implicit val hasParams: HasParams.Aux[this.type, Unused.type] =
      new HasParams[this.type] {
        type Params = Unused.type
      }
    implicit val hasResult: HasResult.Aux[this.type, Unused.type] =
      new HasResult[this.type] {
        type Result = Unused.type
      }
  }

  case object ProfilingStop extends Method("profiling/stop") {

    implicit val hasParams: HasParams.Aux[this.type, Unused.type] =
      new HasParams[this.type] {
        type Params = Unused.type
      }
    implicit val hasResult: HasResult.Aux[this.type, Unused.type] =
      new HasResult[this.type] {
        type Result = Unused.type
      }
  }

}
