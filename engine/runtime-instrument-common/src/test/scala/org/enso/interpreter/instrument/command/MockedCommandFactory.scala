package org.enso.interpreter.instrument.command
import org.enso.polyglot.runtime.Runtime.Api

class MockedCommandFactory extends CommandFactory {

  private var editRequestCounter         = 0
  private var attachVisualizationCounter = 0

  override def createCommand(request: Api.Request): Command = {
    request.payload match {
      case payload: Api.EditFileNotification =>
        val cmd = new SlowEditFileCmd(payload, editRequestCounter % 2 == 0)
        editRequestCounter += 1
        cmd
      case payload: Api.AttachVisualization =>
        val cmd = new SlowAttachVisualizationCmd(
          request.requestId,
          payload,
          attachVisualizationCounter % 2 == 0
        )
        attachVisualizationCounter += 1
        cmd
      case _ =>
        super.createCommand(request)
    }
  }

}
