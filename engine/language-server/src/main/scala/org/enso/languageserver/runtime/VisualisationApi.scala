package org.enso.languageserver.runtime

import java.util.UUID

import org.enso.jsonrpc.{HasParams, HasResult, Method, Unused}

/** The visualisation JSON RPC API provided by the language server.
  *
  * @see [[https://github.com/enso-org/enso/blob/main/docs/language-server/README.md]]
  */
object VisualisationApi {

  case object ExecuteExpression
      extends Method("executionContext/executeExpression") {

    case class Params(
      visualisationId: UUID,
      expressionId: UUID,
      visualisationConfig: VisualisationConfiguration
    )

    implicit val hasParams = new HasParams[this.type] {
      type Params = ExecuteExpression.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = Unused.type
    }
  }

  case object AttachVisualisation
      extends Method("executionContext/attachVisualisation") {

    case class Params(
      visualisationId: UUID,
      expressionId: UUID,
      visualisationConfig: VisualisationConfiguration
    )

    implicit val hasParams = new HasParams[this.type] {
      type Params = AttachVisualisation.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = Unused.type
    }
  }

  case object DetachVisualisation
      extends Method("executionContext/detachVisualisation") {

    case class Params(
      contextId: UUID,
      visualisationId: UUID,
      expressionId: UUID
    )

    implicit val hasParams = new HasParams[this.type] {
      type Params = DetachVisualisation.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = Unused.type
    }

  }

  case object ModifyVisualisation
      extends Method("executionContext/modifyVisualisation") {

    case class Params(
      visualisationId: UUID,
      visualisationConfig: VisualisationConfiguration
    )

    implicit val hasParams = new HasParams[this.type] {
      type Params = ModifyVisualisation.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = Unused.type
    }
  }

}
