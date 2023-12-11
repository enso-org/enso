package org.enso.interpreter.test.instrument

import akka.actor.ActorSystem
import org.enso.distribution.locking.ThreadSafeLockManager
import org.enso.lockmanager.server.LockManagerService
import org.enso.polyglot.RuntimeServerInfo
import org.enso.polyglot.runtime.Runtime.Api
import org.graalvm.polyglot.io.{MessageEndpoint, MessageTransport}

import java.nio.ByteBuffer
import java.util.concurrent.LinkedBlockingQueue

/** Emulates the language server for the purposes of testing.
  *
  * Runtime tests are run in the absence of a real language server, which is
  * only simulated by manually sending requests to the runtime and checking the
  * answers.
  *
  * However the runtime can also send requests (currently related to the locking
  * mechanism) to the language server and if no language server is present,
  * these requests would go unhandled and stall the tests (because they would
  * wait forever on the locks). To fix this issue, this class emulates the
  * request-handling part of the language server by forwarding the lock-related
  * requests to a provided lock manager. Any other communication (like runtime's
  * responses to fake language server requests) are directed to the provided
  * message queue, so that they can be inspected by the tests. The lock-related
  * communication is not forwarded to the message queue, as it is fully handled
  * by this class.
  *
  * @param messageQueue the queue on which runtime's responses are pushed
  * @param lockManager the lock manager to use for handling the lock-related
  *                    requests
  */
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

  /** Sends a message to the runtime. */
  def sendToRuntime(msg: Api.Request): Unit =
    endpoint.sendBinary(Api.serialize(msg))

  /** Creates a [[MessageTransport]] that should be provided when building the
    * context.
    */
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

  def terminate() = system.terminate()
}
