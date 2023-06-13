package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.polyglot.runtime.Runtime.Api.RequestId

import scala.concurrent.{ExecutionContext, Future}

/** A command that gets the component groups of the libraries in scope.
  *
  * @param maybeRequestId an option with request id
  */
class GetComponentGroupsCmd(
  maybeRequestId: Option[RequestId]
) extends AsynchronousCommand(maybeRequestId) {

  /** @inheritdoc */
  override def executeAsynchronously(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] = Future {
    val components =
      ctx.executionService.getContext.getPackageRepository.getComponents
    reply(Api.GetComponentGroupsResponse(components.toVector))
  }
}
