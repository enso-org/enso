package org.enso.interpreter.instrument

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
