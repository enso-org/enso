package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.{Completion, RuntimeContext}
import org.enso.polyglot.runtime.Runtime.{Api, ApiNotification, ApiResponse}
import org.enso.polyglot.runtime.Runtime.Api.RequestId

import scala.concurrent.ExecutionContext

/** Base command trait that encapsulates a function request. Uses
  * [[RuntimeContext]] to perform a request.
  */
abstract class Command(maybeRequestId: Option[RequestId]) {

  type Result[_]

  /** Executes a request.
    *
    * @param ctx contains suppliers of services to perform a request
    */
  def execute(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Result[Completion]

  override def toString: String = this.getClass.getSimpleName

  protected def reply(
    payload: ApiResponse
  )(implicit ctx: RuntimeContext): Unit = {
    ctx.endpoint.sendToClient(Api.Response(maybeRequestId, payload))
  }

  protected def notify(
    payload: ApiNotification
  )(implicit ctx: RuntimeContext): Unit = {
    ctx.endpoint.sendToClient(Api.Response(None, payload))
  }
}
