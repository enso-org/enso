package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.polyglot.runtime.Runtime.Api

import scala.concurrent.{ExecutionContext, Future}

/**
  * A command that invalidates the modules index.
  *
  * @param maybeRequestId an option with request id
  * @param request a request for invalidation
  */
class InvalidateModulesIndexCmd(
  maybeRequestId: Option[Api.RequestId],
  val request: Api.InvalidateModulesIndexRequest
) extends Command(maybeRequestId) {

  /**
    * Executes a request.
    *
    * @param ctx contains suppliers of services to perform a request
    */
  override def execute(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] = {
    for {
      _ <- Future { ctx.jobControlPlane.abortAllJobs() }
    } yield {
      ctx.executionService.getContext.getTopScope.getModules
        .forEach(_.setIndexed(false))
      reply(Api.InvalidateModulesIndexResponse())
    }
  }

}
