package org.enso.interpreter.instrument

import com.oracle.truffle.api.TruffleContext
import org.enso.interpreter.instrument.command.{
  CommandFactory,
  CommandFactoryImpl
}
import org.enso.interpreter.instrument.execution.{
  CommandExecutionEngine,
  CommandProcessor
}
import org.enso.interpreter.service.ExecutionService
import org.enso.polyglot.runtime.Runtime.Api

/** A message handler, dispatching behaviors based on messages received
  * from an instance of [[Endpoint]].
  */
abstract class Handler {
  val endpoint       = new Endpoint(this)
  val contextManager = new ExecutionContextManager

  private case class HandlersContext(
    executionService: ExecutionService,
    sequentialExecutionService: ExecutionService,
    truffleContext: TruffleContext,
    commandProcessor: CommandProcessor
  )

  @volatile private var ctx: HandlersContext = _

  /** Initializes the handler with relevant Truffle objects, allowing it to
    * perform code execution.
    *
    * @param executionService the language execution service instance.
    * @param truffleContext the current Truffle context.
    */
  def initializeExecutionService(
    executionService: ExecutionService,
    truffleContext: TruffleContext
  ): Unit = {
    val interpreterCtx =
      InterpreterContext(
        executionService,
        contextManager,
        endpoint,
        truffleContext
      )
    val commandProcessor = new CommandExecutionEngine(interpreterCtx)
    ctx = HandlersContext(
      executionService,
      executionService,
      truffleContext,
      commandProcessor
    )
    executionService.initializeLanguageServerConnection(endpoint)
    endpoint.sendToClient(Api.Response(Api.InitializedNotification()))
  }

  /** Handles a message received from the client.
    *
    * @param request the message to handle.
    */
  def onMessage(request: Api.Request): Unit = {
    val localCtx = ctx
    request match {
      case Api.Request(requestId, Api.ShutDownRuntimeServer()) =>
        if (localCtx != null) {
          localCtx.commandProcessor.stop()
        }
        endpoint.sendToClient(
          Api.Response(requestId, Api.RuntimeServerShutDown())
        )

      case request: Api.Request =>
        if (localCtx != null) {
          val cmd = cmdFactory.createCommand(request)
          localCtx.commandProcessor.invoke(cmd)
        } else {
          throw new IllegalStateException(
            "received a request to handle with interpreter context not being initialized"
          )
        }
    }
  }

  def cmdFactory: CommandFactory
}

private class HandlerImpl extends Handler {
  override def cmdFactory: CommandFactory = CommandFactoryImpl
}
