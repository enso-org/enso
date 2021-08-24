package org.enso.lockmanager.client
import com.typesafe.scalalogging.Logger
import org.enso.polyglot.runtime.Runtime.{Api, ApiRequest, ApiResponse}

import java.util.UUID
import scala.concurrent.{Future, Promise}
import scala.util.Success

abstract class RuntimeServerRequestHandler
    extends RuntimeServerConnectionEndpoint {
  private val knownRequests
    : collection.concurrent.Map[UUID, Promise[ApiResponse]] =
    collection.concurrent.TrieMap.empty

  private lazy val logger = Logger[this.type]

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

  private def registerPromise(
    requestId: UUID,
    promise: Promise[ApiResponse]
  ): Unit = knownRequests.put(requestId, promise)

  def sendToClient(request: Api.Request): Unit

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
