package org.enso.languageserver.runtime

import org.enso.jsonrpc.{HasParams, HasResult, Method, Unused}
import org.enso.languageserver.libraries.LibraryComponentGroup

object RuntimeApi {

  case object RuntimeGetComponentGroups
      extends Method("runtime/getComponentGroups") {

    case class Result(componentGroups: Seq[LibraryComponentGroup])

    implicit val hasParams: HasParams.Aux[this.type, Unused.type] =
      new HasParams[this.type] {
        type Params = Unused.type
      }
    implicit val hasResult
      : HasResult.Aux[this.type, RuntimeGetComponentGroups.Result] =
      new HasResult[this.type] {
        type Result = RuntimeGetComponentGroups.Result
      }
  }
}
