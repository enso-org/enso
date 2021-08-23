package org.enso.interpreter.instrument

import com.oracle.truffle.api.{TruffleContext, TruffleLogger}
import org.enso.interpreter.instrument.command.CommandFactory
import org.enso.interpreter.instrument.execution.{
  CommandExecutionEngine,
  CommandProcessor
}
import org.enso.interpreter.service.ExecutionService
import org.enso.lockmanager.client.RuntimeServerConnectionEndpoint
import org.enso.polyglot.RuntimeServerInfo
import org.enso.polyglot.runtime.Runtime.{
  Api,
  ApiEnvelope,
  ApiRequest,
  ApiResponse
}
import org.graalvm.polyglot.io.MessageEndpoint

import java.nio.ByteBuffer
import java.util.UUID
import scala.concurrent.{Future, Promise}
import scala.util.Success

/** A message endpoint implementation used by the
  * [[org.enso.interpreter.instrument.RuntimeServerInstrument]].
  */
class Endpoint(handler: Handler)
    extends MessageEndpoint
    with RuntimeServerConnectionEndpoint {

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

  /** Sends a request to the connected client and expects a reply. */
  def sendRequest(msg: ApiRequest): Future[ApiResponse] = {
    val promise = Promise[ApiResponse]()
    val uuid    = UUID.randomUUID()
    handler.registerPromise(uuid, promise)
    val request = Api.Request(uuid, msg)
    client.sendBinary(Api.serialize(request))
    promise.future
  }

  override def sendText(text: String): Unit = {}

  override def sendBinary(data: ByteBuffer): Unit =
    Api.deserializeApiEnvelope(data).foreach(handler.onMessage)

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

  var executionService: ExecutionService = _
  var truffleContext: TruffleContext     = _
  var commandProcessor: CommandProcessor = _

  private val knownRequests
    : collection.concurrent.Map[UUID, Promise[ApiResponse]] =
    collection.concurrent.TrieMap.empty

  private lazy val logger =
    TruffleLogger.getLogger(RuntimeServerInfo.INSTRUMENT_NAME)

  /** Initializes the handler with relevant Truffle objects, allowing it to
    * perform code execution.
    *
    * @param service the language execution service instance.
    * @param context the current Truffle context.
    */
  def initializeExecutionService(
    service: ExecutionService,
    context: TruffleContext
  ): Unit = {
    executionService = service
    truffleContext   = context
    val interpreterCtx =
      InterpreterContext(
        executionService,
        contextManager,
        endpoint,
        truffleContext
      )
    commandProcessor = new CommandExecutionEngine(interpreterCtx)
    executionService.initializeLanguageServerConnection(endpoint)
    endpoint.sendToClient(Api.Response(Api.InitializedNotification()))
  }

  def registerPromise(requestId: UUID, promise: Promise[ApiResponse]): Unit =
    knownRequests.put(requestId, promise)

  /** Handles a message received from the client.
    *
    * @param request the message to handle.
    */
  def onMessage(request: ApiEnvelope): Unit = request match {
    case Api.Request(requestId, Api.ShutDownRuntimeServer()) =>
      commandProcessor.stop()
      endpoint.sendToClient(
        Api.Response(requestId, Api.RuntimeServerShutDown())
      )

    case request: Api.Request =>
      val cmd = CommandFactory.createCommand(request)
      commandProcessor.invoke(cmd)

    case Api.Response(None, payload) =>
      logger.warning(
        s"Received a notification [$payload], but passing notifications from " +
        s"the Language Server to the Runtime is currently not supported."
      )

    case Api.Response(Some(correlationId), payload) =>
      knownRequests.remove(correlationId) match {
        case Some(promise) =>
          promise.complete(Success(payload))
        case None =>
          logger.warning(
            s"Received a response to an unknown request: [$correlationId]."
          )
      }
  }

}
