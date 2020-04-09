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

  type ContextId = UUID

  case object ExecutionContextCreate extends Method("executionContext/create") {

    case class Result(
      canModify: CapabilityRegistration,
      receivesEvents: CapabilityRegistration
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

  case object StackItemNotFoundError extends Error(2001, "Stack item not found")

  case object ContextNotFoundError extends Error(2002, "Context not found")

  case object EmptyStackError extends Error(2003, "Stack is empty")

}
