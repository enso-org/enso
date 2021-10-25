package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.polyglot.runtime.Runtime.{Api, ApiResponse}
import org.enso.polyglot.runtime.Runtime.Api.RequestId

import scala.concurrent.{ExecutionContext, Future}

/** Base command trait that encapsulates a function request. Uses
  * [[RuntimeContext]] to perform a request.
  */
abstract class Command(maybeRequestId: Option[RequestId]) {

  /** Executes a request.
    *
    * @param ctx contains suppliers of services to perform a request
    */
  def execute(implicit ctx: RuntimeContext, ec: ExecutionContext): Future[Unit]

  override def toString: String = this.getClass.getSimpleName

  protected def reply(
    payload: ApiResponse
  )(implicit ctx: RuntimeContext): Unit = {
    ctx.endpoint.sendToClient(Api.Response(maybeRequestId, payload))
  }

}
