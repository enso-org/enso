package org.enso.interpreter.instrument.command
import org.enso.polyglot.runtime.Runtime.Api

object MockedCommandFactory extends CommandFactory {

  private var editRequestCounter = 0

  override def createCommand(request: Api.Request): Command = {
    request.payload match {
      case payload: Api.EditFileNotification =>
        val cmd = new SlowEditFileCmd(payload, editRequestCounter)
        editRequestCounter += 1
        cmd
      case _ =>
        super.createCommand(request)
    }
  }

}
