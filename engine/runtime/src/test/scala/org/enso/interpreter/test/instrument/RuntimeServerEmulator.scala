package org.enso.interpreter.test.instrument

import akka.actor.ActorSystem
import org.enso.distribution.locking.ThreadSafeLockManager
import org.enso.lockmanager.server.LockManagerService
import org.enso.polyglot.RuntimeServerInfo
import org.enso.polyglot.runtime.Runtime.Api
import org.graalvm.polyglot.io.{MessageEndpoint, MessageTransport}

import java.nio.ByteBuffer
import java.util.concurrent.LinkedBlockingQueue

class RuntimeServerEmulator(
  messageQueue: LinkedBlockingQueue[Api.Response],
  lockManager: ThreadSafeLockManager
) {
  private val system: ActorSystem       = ActorSystem("TestSystem")
  private var endpoint: MessageEndpoint = _

  private val lockManagerService =
    system.actorOf(LockManagerService.props(lockManager))

  private val connector = system.actorOf(
    TestRuntimeServerConnector.props(
      lockManagerService,
      { response => endpoint.sendBinary(Api.serialize(response)) }
    )
  )

  def sendToRuntime(msg: Api.Request): Unit =
    endpoint.sendBinary(Api.serialize(msg))

  def makeServerTransport: MessageTransport = { (uri, peer) =>
    if (uri.toString == RuntimeServerInfo.URI) {
      endpoint = peer
      new MessageEndpoint {
        override def sendText(text: String): Unit = {}

        override def sendBinary(data: ByteBuffer): Unit = {
          Api.deserializeApiEnvelope(data) match {
            case Some(request: Api.Request) =>
              connector ! request
            case Some(response: Api.Response) =>
              messageQueue.add(response)
            case None =>
              println("Failed to deserialize a message.")
          }
        }

        override def sendPing(data: ByteBuffer): Unit = {}

        override def sendPong(data: ByteBuffer): Unit = {}

        override def sendClose(): Unit = {}
      }
    } else null
  }
}
