package org.enso.lockmanager.client
import com.typesafe.scalalogging.Logger
import org.enso.polyglot.runtime.Runtime.{Api, ApiRequest, ApiResponse}

import java.util.UUID
import scala.concurrent.{Future, Promise}
import scala.util.Success

/** Encapsulates the logic of keeping track of sent requests and completing
  * their futures with corresponding replies to implement the
  * [[RuntimeServerConnectionEndpoint]] interface.
  */
abstract class RuntimeServerRequestHandler
    extends RuntimeServerConnectionEndpoint {

  /** Keeps requests that are currently in-flight, mapping their identifiers to
    * their corresponding promises.
    */
  private val knownRequests
    : collection.concurrent.Map[UUID, Promise[ApiResponse]] =
    collection.concurrent.TrieMap.empty

  private lazy val logger = Logger[this.type]

  /** @inheritdoc */
  override def sendRequest(
    msg: ApiRequest
  ): Future[ApiResponse] = {
    val promise = Promise[ApiResponse]()
    val uuid    = UUID.randomUUID()
    registerPromise(uuid, promise)
    val request = Api.Request(uuid, msg)
    sendToClient(request)
    promise.future
  }

  /** Registers a promise associated with a request id. */
  private def registerPromise(
    requestId: UUID,
    promise: Promise[ApiResponse]
  ): Unit = knownRequests.put(requestId, promise)

  /** The method that must be overridden by the user which defines how the
    * requests are actually sent to the Language Server.
    */
  def sendToClient(request: Api.Request): Unit

  /** The method that must be called by the user when any response is received
    * from the language server.
    *
    * It checks the [[knownRequests]] and completes the corresponding promise
    * with the response.
    */
  def onResponseReceived(response: Api.Response): Unit = response match {
    case Api.Response(None, payload) =>
      logger.warn(
        s"Received a notification [$payload], but passing notifications from " +
        s"the Language Server to the Runtime is currently not supported."
      )

    case Api.Response(Some(correlationId), payload) =>
      knownRequests.remove(correlationId) match {
        case Some(promise) =>
          promise.complete(Success(payload))
        case None =>
          logger.warn(
            s"Received a response to an unknown request: [$correlationId]."
          )
      }
  }
}
