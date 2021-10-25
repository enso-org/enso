package org.enso.languageserver.requesthandler.capability

import akka.actor.{Actor, ActorRef, Cancellable, Props}
import com.typesafe.scalalogging.LazyLogging
import org.enso.jsonrpc._
import org.enso.languageserver.capability.CapabilityApi.AcquireCapability
import org.enso.languageserver.capability.CapabilityProtocol
import org.enso.languageserver.capability.CapabilityProtocol.{
  CapabilityAcquired,
  CapabilityAcquisitionBadRequest,
  CapabilityAcquisitionFileSystemFailure
}
import org.enso.languageserver.data.CapabilityRegistration
import org.enso.languageserver.filemanager.FileSystemFailureMapper
import org.enso.languageserver.requesthandler.RequestTimeout
import org.enso.languageserver.session.JsonSession
import org.enso.languageserver.util.UnhandledLogging

import scala.concurrent.duration.FiniteDuration

/** A request handler for `capability/acquire` commands.
  *
  * @param capabilityRouter a router that dispatches capability requests
  * @param timeout a request timeout
  * @param session an object representing a client connected to the language server
  */
class AcquireCapabilityHandler(
  capabilityRouter: ActorRef,
  timeout: FiniteDuration,
  session: JsonSession
) extends Actor
    with LazyLogging
    with UnhandledLogging {

  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    case Request(AcquireCapability, id, registration: CapabilityRegistration) =>
      capabilityRouter ! CapabilityProtocol.AcquireCapability(
        session,
        registration
      )
      val cancellable =
        context.system.scheduler.scheduleOnce(timeout, self, RequestTimeout)
      context.become(responseStage(id, sender(), cancellable))
  }

  private def responseStage(
    id: Id,
    replyTo: ActorRef,
    cancellable: Cancellable
  ): Receive = {
    case RequestTimeout =>
      logger.error(
        "Acquiring capability for client [{}] timed out.",
        session.clientId
      )
      replyTo ! ResponseError(Some(id), Errors.RequestTimeout)
      context.stop(self)

    case CapabilityAcquired =>
      replyTo ! ResponseResult(AcquireCapability, id, Unused)
      cancellable.cancel()
      context.stop(self)

    case CapabilityAcquisitionBadRequest =>
      replyTo ! ResponseError(Some(id), Errors.ServiceError)
      cancellable.cancel()
      context.stop(self)

    case CapabilityAcquisitionFileSystemFailure(error) =>
      replyTo ! ResponseError(
        Some(id),
        FileSystemFailureMapper.mapFailure(error)
      )
      cancellable.cancel()
      context.stop(self)
  }
}

object AcquireCapabilityHandler {

  /** Creates a configuration object used to create a [[AcquireCapabilityHandler]]
    *
    * @param capabilityRouter a router that dispatches capability requests
    * @param requestTimeout a request timeout
    * @param rpcSession an object representing a client connected to the language server
    * @return a configuration object
    */
  def props(
    capabilityRouter: ActorRef,
    requestTimeout: FiniteDuration,
    rpcSession: JsonSession
  ): Props =
    Props(
      new AcquireCapabilityHandler(capabilityRouter, requestTimeout, rpcSession)
    )

}
