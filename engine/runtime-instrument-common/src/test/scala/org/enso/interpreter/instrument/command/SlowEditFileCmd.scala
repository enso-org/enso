package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.polyglot.runtime.Runtime.Api

import scala.concurrent.ExecutionContext

class SlowEditFileCmd(request: Api.EditFileNotification, delay: Boolean)
    extends EditFileCmd(request) {

  override def executeSynchronously(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Unit = {
    if (
      ctx.executionService.getContext.isRandomDelayedCommandExecution && delay
    ) {
      try {
        Thread.sleep(2000)
      } catch {
        case _: InterruptedException =>
      }
    }
    super.executeSynchronously(ctx, ec)
  }
}
