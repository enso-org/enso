package org.enso.interpreter.instrument

import java.nio.ByteBuffer

import com.oracle.truffle.api.TruffleContext
import org.enso.interpreter.instrument.command.CommandFactory
import org.enso.interpreter.instrument.execution.{
  CommandProcessor,
  PreemptiveCommandProcessor,
  RuntimeContext
}
import org.enso.interpreter.service.ExecutionService
import org.enso.polyglot.runtime.Runtime.Api
import org.graalvm.polyglot.io.MessageEndpoint

import scala.concurrent.Await
import scala.concurrent.duration._

/**
  * A message endpoint implementation used by the
  * [[org.enso.interpreter.instrument.RuntimeServerInstrument]].
  */
class Endpoint(handler: Handler) extends MessageEndpoint {

  var client: MessageEndpoint = _

  /**
    * Sets the client end of the connection, after it has been established.
    *
    * @param ep the client endpoint.
    */
  def setClient(ep: MessageEndpoint): Unit = client = ep

  /**
    * Sends a response to the connected client.
    *
    * @param msg the message to send.
    */
  def sendToClient(msg: Api.Response): Unit =
    client.sendBinary(Api.serialize(msg))

  override def sendText(text: String): Unit = {}

  override def sendBinary(data: ByteBuffer): Unit =
    Api.deserializeRequest(data).foreach(handler.onMessage)

  override def sendPing(data: ByteBuffer): Unit = client.sendPong(data)

  override def sendPong(data: ByteBuffer): Unit = {}

  override def sendClose(): Unit = {}
}

/**
  * A message handler, dispatching behaviors based on messages received
  * from an instance of [[Endpoint]].
  */
final class Handler {
  val endpoint       = new Endpoint(this)
  val contextManager = new ExecutionContextManager

  var executionService: ExecutionService = _
  var truffleContext: TruffleContext     = _
  var commandProcessor: CommandProcessor = _

  /**
    * Initializes the handler with relevant Truffle objects, allowing it to
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
    endpoint.sendToClient(Api.Response(Api.InitializedNotification()))
    commandProcessor =
      new PreemptiveCommandProcessor(1, executionService.getContext)
  }

  /**
    * Handles a message received from the client.
    *
    * @param msg the message to handle.
    */
  def onMessage(msg: Api.Request): Unit = {
    val cmd = CommandFactory.createCommand(msg)
    val ctx = RuntimeContext(
      executionService,
      contextManager,
      endpoint,
      truffleContext,
      commandProcessor
    )
    val future = commandProcessor.invoke(cmd, ctx)
    Await.result(future, 1.minute)
  }

}
