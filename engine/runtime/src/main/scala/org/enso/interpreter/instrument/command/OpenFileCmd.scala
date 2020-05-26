package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.polyglot.runtime.Runtime.Api

/**
  * A command that opens a file.
  *
  * @param request a request for a service
  */
class OpenFileCmd(request: Api.OpenFileNotification) extends Command {

  /** @inheritdoc **/
  override def execute(implicit ctx: RuntimeContext): Unit = {
    ctx.executionService.setModuleSources(request.path, request.contents)
  }

}
