package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.polyglot.runtime.Runtime.Api

import scala.concurrent.ExecutionContext

class SlowEditFileCmd(request: Api.EditFileNotification, counter: Int)
    extends EditFileCmd(request) {

  override def executeSynchronously(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Unit = {
    if (
      ctx.executionService.getContext.isRandomDelayedCommandExecution && counter % 2 == 0
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
