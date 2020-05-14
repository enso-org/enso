package org.enso.languageserver.runtime

import java.util.UUID

import org.enso.jsonrpc.{Error, HasParams, HasResult, Method, Unused}
import org.enso.languageserver.data.CapabilityRegistration

/**
  * The execution JSON RPC API provided by the language server.
  *
  * @see [[https://github.com/luna/enso/blob/master/doc/design/engine/engine-services.md]]
  */
object ExecutionApi {

  type ContextId    = UUID
  type ExpressionId = UUID

  case object ExecutionContextCreate extends Method("executionContext/create") {

    case class Result(
      contextId: ContextId,
      canModify: CapabilityRegistration,
      receivesUpdates: CapabilityRegistration
    )

    implicit val hasParams = new HasParams[this.type] {
      type Params = Unused.type
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = ExecutionContextCreate.Result
    }
  }

  case object ExecutionContextDestroy
      extends Method("executionContext/destroy") {

    case class Params(contextId: ContextId)

    implicit val hasParams = new HasParams[this.type] {
      type Params = ExecutionContextDestroy.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = Unused.type
    }
  }

  case object ExecutionContextPush extends Method("executionContext/push") {

    case class Params(contextId: ContextId, stackItem: StackItem)

    implicit val hasParams = new HasParams[this.type] {
      type Params = ExecutionContextPush.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = Unused.type
    }
  }

  case object ExecutionContextPop extends Method("executionContext/pop") {

    case class Params(contextId: ContextId)

    implicit val hasParams = new HasParams[this.type] {
      type Params = ExecutionContextPop.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = Unused.type
    }
  }

  case object ExecutionContextRecompute
      extends Method("executionContext/recompute") {

    case class Params(
      contextId: ContextId,
      invalidatedExpressions: Option[InvalidatedExpressions]
    )

    implicit val hasParams = new HasParams[this.type] {
      type Params = ExecutionContextRecompute.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = Unused.type
    }
  }

  case object ExecutionContextExpressionValuesComputed
      extends Method("executionContext/expressionValuesComputed") {

    case class Params(
      contextId: ContextId,
      updates: Vector[ExpressionValueUpdate]
    )

    implicit val hasParams = new HasParams[this.type] {
      type Params = ExecutionContextExpressionValuesComputed.Params
    }
  }

  case object StackItemNotFoundError extends Error(2001, "Stack item not found")

  case object ContextNotFoundError extends Error(2002, "Context not found")

  case object EmptyStackError extends Error(2003, "Stack is empty")

  case object InvalidStackItemError extends Error(2004, "Invalid stack item")

  case class ModuleNotFoundError(moduleName: String)
      extends Error(2005, s"Module not found [$moduleName]")

  case object VisualisationNotFoundError
      extends Error(2006, s"Visualisation not found")

  case class VisualisationExpressionError(msg: String)
      extends Error(
        2007,
        s"Evaluation of the visualisation expression failed [$msg]"
      )
  case class VisualisationEvaluationError(msg: String)
      extends Error(2008, s"Evaluation of the visualisation failed [$msg]")

  case class ExecutionFailedError(msg: String)
      extends Error(2009, s"Execution failed [$msg]")
}
