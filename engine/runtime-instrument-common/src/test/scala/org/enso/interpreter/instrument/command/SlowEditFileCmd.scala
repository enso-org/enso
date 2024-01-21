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
    if (counter % 2 == 1) {
      Thread.sleep(2000)
    }
    super.executeSynchronously(ctx, ec)
  }
}
