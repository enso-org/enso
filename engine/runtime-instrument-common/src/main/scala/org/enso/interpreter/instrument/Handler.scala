package org.enso.interpreter.instrument

import com.oracle.truffle.api.TruffleContext
import org.enso.interpreter.instrument.command.CommandFactory
import org.enso.interpreter.instrument.execution.{
  CommandExecutionEngine,
  CommandProcessor
}
import org.enso.interpreter.service.ExecutionService
import org.enso.lockmanager.client.{
  RuntimeServerConnectionEndpoint,
  RuntimeServerRequestHandler
}
import org.enso.polyglot.runtime.Runtime.{Api, ApiRequest, ApiResponse}
import org.graalvm.polyglot.io.MessageEndpoint

import java.nio.ByteBuffer
import scala.concurrent.Future

/** A message endpoint implementation. */
class Endpoint(handler: Handler)
    extends MessageEndpoint
    with RuntimeServerConnectionEndpoint {

  /** A helper endpoint that is used for handling requests sent to the Language
    * Server.
    */
  private val reverseRequestEndpoint = new RuntimeServerRequestHandler {
    override def sendToClient(request: Api.Request): Unit =
      client.sendBinary(Api.serialize(request))
  }

  var client: MessageEndpoint = _

  /** Sets the client end of the connection, after it has been established.
    *
    * @param ep the client endpoint.
    */
  def setClient(ep: MessageEndpoint): Unit = client = ep

  /** Sends a response to the connected client.
    *
    * @param msg the message to send.
    */
  def sendToClient(msg: Api.Response): Unit =
    client.sendBinary(Api.serialize(msg))

  /** Sends a notification to the runtime.
    *
    * Can be used to start a command processing in the background.
    *
    * @param msg the message to send.
    */
  def sendToSelf(msg: Api.Request): Unit =
    handler.onMessage(msg)

  /** Sends a request to the connected client and expects a reply. */
  override def sendRequest(msg: ApiRequest): Future[ApiResponse] =
    reverseRequestEndpoint.sendRequest(msg)

  override def sendText(text: String): Unit = {}

  override def sendBinary(data: ByteBuffer): Unit =
    Api.deserializeApiEnvelope(data).foreach {
      case request: Api.Request =>
        handler.onMessage(request)
      case response: Api.Response =>
        reverseRequestEndpoint.onResponseReceived(response)
    }

  override def sendPing(data: ByteBuffer): Unit = client.sendPong(data)

  override def sendPong(data: ByteBuffer): Unit = {}

  override def sendClose(): Unit = {}
}

/** A message handler, dispatching behaviors based on messages received
  * from an instance of [[Endpoint]].
  */
final class Handler {
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
          val cmd = CommandFactory.createCommand(request)
          localCtx.commandProcessor.invoke(cmd)
        } else {
          throw new IllegalStateException(
            "received a request to handle with interpreter context not being initialized"
          )
        }
    }
  }
}
