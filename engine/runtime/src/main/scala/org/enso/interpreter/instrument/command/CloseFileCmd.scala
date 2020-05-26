package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.polyglot.runtime.Runtime.Api

/**
  * A command that closes a file.
  *
  * @param request a request for a service
  */
class CloseFileCmd(request: Api.CloseFileNotification) extends Command {

  /** @inheritdoc **/
  override def execute(implicit ctx: RuntimeContext): Unit = {
    ctx.executionService.resetModuleSources(request.path)
  }

}
