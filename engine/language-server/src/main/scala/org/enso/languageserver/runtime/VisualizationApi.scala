package org.enso.languageserver.runtime

import java.util.UUID

import org.enso.jsonrpc.{HasParams, HasResult, Method, Unused}

/** The visualization JSON RPC API provided by the language server.
  *
  * @see [[https://github.com/enso-org/enso/blob/develop/docs/language-server/README.md]]
  */
object VisualizationApi {

  case object ExecuteExpression
      extends Method("executionContext/executeExpression") {

    case class Params(
      visualizationId: UUID,
      expressionId: UUID,
      visualizationConfig: VisualizationConfiguration
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

  case object AttachVisualization
      extends Method("executionContext/attachVisualization") {

    case class Params(
      visualizationId: UUID,
      expressionId: UUID,
      visualizationConfig: VisualizationConfiguration
    )

    implicit
    val hasParams: HasParams.Aux[this.type, AttachVisualization.Params] =
      new HasParams[this.type] {
        type Params = AttachVisualization.Params
      }
    implicit val hasResult: HasResult.Aux[this.type, Unused.type] =
      new HasResult[this.type] {
        type Result = Unused.type
      }
  }

  case object DetachVisualization
      extends Method("executionContext/detachVisualization") {

    case class Params(
      contextId: UUID,
      visualizationId: UUID,
      expressionId: UUID
    )

    implicit
    val hasParams: HasParams.Aux[this.type, DetachVisualization.Params] =
      new HasParams[this.type] {
        type Params = DetachVisualization.Params
      }
    implicit val hasResult: HasResult.Aux[this.type, Unused.type] =
      new HasResult[this.type] {
        type Result = Unused.type
      }
  }

  case object ModifyVisualization
      extends Method("executionContext/modifyVisualization") {

    case class Params(
      visualizationId: UUID,
      visualizationConfig: VisualizationConfiguration
    )

    implicit
    val hasParams: HasParams.Aux[this.type, ModifyVisualization.Params] =
      new HasParams[this.type] {
        type Params = ModifyVisualization.Params
      }
    implicit val hasResult: HasResult.Aux[this.type, Unused.type] =
      new HasResult[this.type] {
        type Result = Unused.type
      }
  }

}
