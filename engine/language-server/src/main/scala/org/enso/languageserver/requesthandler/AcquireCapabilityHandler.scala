package org.enso.languageserver.requesthandler

import akka.actor.{Actor, ActorLogging, ActorRef, Cancellable, Props}
import org.enso.jsonrpc.Errors.ServiceError
import org.enso.jsonrpc._
import org.enso.languageserver.capability.CapabilityApi.AcquireCapability
import org.enso.languageserver.capability.CapabilityProtocol
import org.enso.languageserver.capability.CapabilityProtocol.{
  CapabilityAcquired,
  CapabilityAcquisitionBadRequest,
  CapabilityAcquisitionFileSystemFailure
}
import org.enso.languageserver.data.{CapabilityRegistration, Client}
import org.enso.languageserver.filemanager.FileSystemFailureMapper

import scala.concurrent.duration.FiniteDuration

/**
  * A request handler for `capability/acquire` commands.
  *
  * @param capabilityRouter a router that dispatches capability requests
  * @param timeout a request timeout
  * @param client an object representing a client connected to the language server
  */
class AcquireCapabilityHandler(
  capabilityRouter: ActorRef,
  timeout: FiniteDuration,
  client: Client
) extends Actor
    with ActorLogging {

  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    case Request(AcquireCapability, id, registration: CapabilityRegistration) =>
      capabilityRouter ! CapabilityProtocol.AcquireCapability(
        client,
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
      log.error(s"Acquiring capability for ${client.id} timed out")
      replyTo ! ResponseError(Some(id), ServiceError)
      context.stop(self)

    case CapabilityAcquired =>
      replyTo ! ResponseResult(AcquireCapability, id, Unused)
      cancellable.cancel()
      context.stop(self)

    case CapabilityAcquisitionBadRequest =>
      replyTo ! ResponseError(Some(id), ServiceError)
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

  override def unhandled(message: Any): Unit =
    log.warning("Received unknown message: {}", message)

}

object AcquireCapabilityHandler {

  /**
    * Creates a configuration object used to create a [[AcquireCapabilityHandler]]
    *
    * @param capabilityRouter a router that dispatches capability requests
    * @param requestTimeout a request timeout
    * @param client an object representing a client connected to the language server
    * @return a configuration object
    */
  def props(
    capabilityRouter: ActorRef,
    requestTimeout: FiniteDuration,
    client: Client
  ): Props =
    Props(
      new AcquireCapabilityHandler(capabilityRouter, requestTimeout, client)
    )

}
