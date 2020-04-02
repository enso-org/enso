package org.enso.interpeter.instrument

import java.nio.ByteBuffer

import org.enso.polyglot.runtime.Runtime.Api
import org.graalvm.polyglot.io.MessageEndpoint

/**
  * A message endpoint implementation used by the
  * [[org.enso.interpreter.instrument.RuntimeServerInstrument]].
  * @param handler
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
    * Sends a message to the connected client.
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
class Handler {
  val endpoint       = new Endpoint(this)
  val contextManager = new ExecutionContextManager

  /**
    * Handles a message received from the client.
    *
    * @param msg the message to handle.
    */
  def onMessage(msg: Api.Request): Unit = msg match {
    case Api.Request(requestId, Api.CreateContextRequest(contextId)) =>
      contextManager.create(contextId)
      endpoint.sendToClient(
        Api.Response(requestId, Api.CreateContextResponse(contextId))
      )

    case Api.Request(requestId, Api.DestroyContextRequest(contextId)) =>
      if (contextManager.get(contextId).isDefined) {
        contextManager.destroy(contextId)
        endpoint.sendToClient(
          Api.Response(requestId, Api.DestroyContextResponse(contextId, None))
        )
      } else {
        endpoint.sendToClient(
          Api.Response(
            requestId,
            Api.DestroyContextResponse(
              contextId,
              Some(Api.ContextDoesNotExistError()))
          )
        )
      }

  }
}
