package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.polyglot.runtime.Runtime.Api

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}

/** A command that verifies the modules index.
  *
  * @param maybeRequestId an option with request id
  * @param request a verification request
  */
class VerifyModulesIndexCmd(
  maybeRequestId: Option[Api.RequestId],
  val request: Api.VerifyModulesIndexRequest
) extends Command(maybeRequestId) {

  /** Executes a request.
    *
    * @param ctx contains suppliers of services to perform a request
    */
  override def execute(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] = {
    ctx.locking.acquireReadCompilationLock()
    try {
      val builder = mutable.Set(request.modules: _*)
      ctx.executionService.getContext.getTopScope.getModules.forEach { module =>
        builder -= module.getName.toString
      }
      Future(reply(Api.VerifyModulesIndexResponse(builder.toVector)))
    } finally {
      ctx.locking.releaseReadCompilationLock()
    }
  }

}
