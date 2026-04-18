package org.enso.languageserver.runtime

import java.util.UUID

import org.enso.jsonrpc.{HasParams, HasResult, Method, Unused}

/** The visualization JSON RPC API provided by the language server.
  *
  * Only `executionContext/executeExpression` remains here. Attach/detach/modify
  * visualizations and visualization update/evaluation-failed notifications have
  * moved off JSON-RPC onto the vis subdoc synchronized via ydoc-server.
  *
  * @see [[https://github.com/enso-org/enso/blob/develop/docs/language-server/README.md]]
  */
object VisualizationApi {

  case object ExecuteExpression
      extends Method("executionContext/executeExpression") {

    case class Params(
      executionContextId: UUID,
      visualizationId: UUID,
      expressionId: UUID,
      expression: String
    )

    implicit val hasParams: HasParams.Aux[this.type, ExecuteExpression.Params] =
      new HasParams[this.type] {
        type Params = ExecuteExpression.Params
      }
    implicit val hasResult: HasResult.Aux[this.type, Unused.type] =
      new HasResult[this.type] {
        type Result = Unused.type
      }
  }

}
