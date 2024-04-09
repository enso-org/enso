package org.enso.languageserver.runtime

import io.circe.generic.auto._
import io.circe.{Decoder, Encoder, Json}
import org.enso.jsonrpc.{Error, HasParams, HasResult, Method, Unused}
import org.enso.languageserver.data.CapabilityRegistration
import org.enso.languageserver.filemanager.Path
import org.enso.languageserver.libraries.LibraryComponentGroup
import org.enso.languageserver.runtime.ContextRegistryProtocol.{
  ExecutionDiagnostic,
  VisualizationContext
}

import java.util.UUID

/** The execution JSON RPC API provided by the language server.
  *
  * @see [[https://github.com/enso-org/enso/blob/develop/docs/language-server/README.md]]
  */
object ExecutionApi {

  type ContextId       = UUID
  type ExpressionId    = UUID
  type VisualizationId = UUID

  case object ExecutionContextCreate extends Method("executionContext/create") {

    case class Params(contextId: Option[ContextId])
    object Params {
      implicit val paramsDecoder: Decoder[Params] =
        Decoder.instance[Params] { cursor =>
          if (cursor.value.isNull) Right(Params(None))
          else {
            for {
              contextId <- cursor.downField("contextId").as[Option[ContextId]]
            } yield Params(contextId)
          }
        }
    }

    case class Result(
      contextId: ContextId,
      canModify: CapabilityRegistration,
      receivesUpdates: CapabilityRegistration
    )

    implicit
    val hasParams: HasParams.Aux[this.type, ExecutionContextCreate.Params] =
      new HasParams[this.type] {
        type Params = ExecutionContextCreate.Params
      }
    implicit val hasResult
      : HasResult.Aux[this.type, ExecutionContextCreate.Result] =
      new HasResult[this.type] {
        type Result = ExecutionContextCreate.Result
      }
  }

  case object ExecutionContextDestroy
      extends Method("executionContext/destroy") {

    case class Params(contextId: ContextId)

    implicit
    val hasParams: HasParams.Aux[this.type, ExecutionContextDestroy.Params] =
      new HasParams[this.type] {
        type Params = ExecutionContextDestroy.Params
      }
    implicit val hasResult: HasResult.Aux[this.type, Unused.type] =
      new HasResult[this.type] {
        type Result = Unused.type
      }
  }

  case object ExecutionContextPush extends Method("executionContext/push") {

    case class Params(contextId: ContextId, stackItem: StackItem)

    implicit
    val hasParams: HasParams.Aux[this.type, ExecutionContextPush.Params] =
      new HasParams[this.type] {
        type Params = ExecutionContextPush.Params
      }
    implicit val hasResult: HasResult.Aux[this.type, Unused.type] =
      new HasResult[this.type] {
        type Result = Unused.type
      }
  }

  case object ExecutionContextPop extends Method("executionContext/pop") {

    case class Params(contextId: ContextId)

    implicit
    val hasParams: HasParams.Aux[this.type, ExecutionContextPop.Params] =
      new HasParams[this.type] {
        type Params = ExecutionContextPop.Params
      }
    implicit val hasResult: HasResult.Aux[this.type, Unused.type] =
      new HasResult[this.type] {
        type Result = Unused.type
      }
  }

  case object ExecutionContextRecompute
      extends Method("executionContext/recompute") {

    case class Params(
      contextId: ContextId,
      invalidatedExpressions: Option[InvalidatedExpressions],
      executionEnvironment: Option[ExecutionEnvironment]
    )

    implicit
    val hasParams: HasParams.Aux[this.type, ExecutionContextRecompute.Params] =
      new HasParams[this.type] {
        type Params = ExecutionContextRecompute.Params
      }
    implicit val hasResult: HasResult.Aux[this.type, Unused.type] =
      new HasResult[this.type] {
        type Result = Unused.type
      }
  }

  case object ExecutionContextInterrupt
      extends Method("executionContext/interrupt") {

    case class Params(contextId: ContextId)

    implicit
    val hasParams: HasParams.Aux[this.type, ExecutionContextInterrupt.Params] =
      new HasParams[this.type] {
        type Params = ExecutionContextInterrupt.Params
      }
    implicit val hasResult: HasResult.Aux[this.type, Unused.type] =
      new HasResult[this.type] {
        type Result = Unused.type
      }
  }

  case object ExecutionContextGetComponentGroups
      extends Method("executionContext/getComponentGroups") {

    case class Params(contextId: ContextId)

    case class Result(componentGroups: Seq[LibraryComponentGroup])

    implicit val hasParams
      : HasParams.Aux[this.type, ExecutionContextGetComponentGroups.Params] =
      new HasParams[this.type] {
        type Params = ExecutionContextGetComponentGroups.Params
      }
    implicit val hasResult
      : HasResult.Aux[this.type, ExecutionContextGetComponentGroups.Result] =
      new HasResult[this.type] {
        type Result = ExecutionContextGetComponentGroups.Result
      }
  }

  case object ExecutionContextExpressionUpdates
      extends Method("executionContext/expressionUpdates") {

    case class Params(
      contextId: ContextId,
      updates: Vector[ContextRegistryProtocol.ExpressionUpdate]
    )

    implicit val hasParams
      : HasParams.Aux[this.type, ExecutionContextExpressionUpdates.Params] =
      new HasParams[this.type] {
        type Params = ExecutionContextExpressionUpdates.Params
      }
  }

  case object ExecutionContextExecutionFailed
      extends Method("executionContext/executionFailed") {

    case class Params(
      contextId: ContextId,
      message: String,
      path: Option[Path]
    )

    implicit val hasParams
      : HasParams.Aux[this.type, ExecutionContextExecutionFailed.Params] =
      new HasParams[this.type] {
        type Params = ExecutionContextExecutionFailed.Params
      }
  }

  case object ExecutionContextExecutionComplete
      extends Method("executionContext/executionComplete") {

    case class Params(contextId: ContextId)

    implicit val hasParams
      : HasParams.Aux[this.type, ExecutionContextExecutionComplete.Params] =
      new HasParams[this.type] {
        type Params = ExecutionContextExecutionComplete.Params
      }
  }

  case object ExecutionContextExecutionStatus
      extends Method("executionContext/executionStatus") {

    case class Params(
      contextId: ContextId,
      diagnostics: Seq[ExecutionDiagnostic]
    )

    implicit val hasParams
      : HasParams.Aux[this.type, ExecutionContextExecutionStatus.Params] =
      new HasParams[this.type] {
        type Params = ExecutionContextExecutionStatus.Params
      }
  }

  case object VisualizationEvaluationFailed
      extends Method("executionContext/visualizationEvaluationFailed") {

    case class Params(
      contextId: ContextId,
      visualizationId: VisualizationId,
      expressionId: ExpressionId,
      message: String,
      diagnostic: Option[ExecutionDiagnostic]
    )

    implicit val hasParams
      : HasParams.Aux[this.type, VisualizationEvaluationFailed.Params] =
      new HasParams[this.type] {
        type Params = VisualizationEvaluationFailed.Params
      }
  }

  case object ExecutionContextSetExecutionEnvironment
      extends Method("executionContext/setExecutionEnvironment") {

    case class Params(
      contextId: ContextId,
      executionEnvironment: ExecutionEnvironment
    )

    implicit val hasParams: HasParams.Aux[
      this.type,
      ExecutionContextSetExecutionEnvironment.Params
    ] = new HasParams[this.type] {
      type Params = ExecutionContextSetExecutionEnvironment.Params
    }
    implicit val hasResult: HasResult.Aux[this.type, Unused.type] =
      new HasResult[this.type] {
        type Result = Unused.type
      }
  }

  case object StackItemNotFoundError extends Error(2001, "Stack item not found")

  case object ContextNotFoundError extends Error(2002, "Context not found")

  case object EmptyStackError extends Error(2003, "Stack is empty")

  case object InvalidStackItemError extends Error(2004, "Invalid stack item")

  case class ModuleNotFoundError(moduleName: String)
      extends Error(2005, s"Module not found [$moduleName]")

  case object VisualizationNotFoundError
      extends Error(2006, s"Visualization not found")

  case class VisualizationExpressionError(
    ctx: VisualizationContext,
    msg: String,
    diagnostic: Option[ContextRegistryProtocol.ExecutionDiagnostic]
  ) extends Error(
        2007,
        s"Evaluation of the visualization expression failed [$msg]"
      ) {

    override def payload: Option[Json] =
      diagnostic.map(
        Encoder[ContextRegistryProtocol.ExecutionDiagnostic].apply(_)
      )
  }
}
