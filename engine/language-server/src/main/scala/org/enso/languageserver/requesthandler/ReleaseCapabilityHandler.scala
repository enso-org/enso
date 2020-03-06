package org.enso.languageserver.requesthandler

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import org.enso.languageserver.capability.CapabilityApi.ReleaseCapability
import org.enso.languageserver.capability.CapabilityProtocol
import org.enso.languageserver.capability.CapabilityProtocol.{
  CapabilityReleaseBadRequest,
  CapabilityReleased
}
import org.enso.languageserver.data.{CapabilityRegistration, Client}
import org.enso.languageserver.jsonrpc.Errors.ServiceError
import org.enso.languageserver.jsonrpc._

import scala.concurrent.duration.FiniteDuration

/**
  * A request handler for `capability/release` commands.
  *
  * @param capabilityRouter a router that dispatches capability requests
  * @param timeout a request timeout
  * @param client an object representing a client connected to the language server
  */
class ReleaseCapabilityHandler(
  capabilityRouter: ActorRef,
  timeout: FiniteDuration,
  client: Client
) extends Actor
    with ActorLogging {
  override def receive: Receive = requestStage

  import context.dispatcher

  private def requestStage: Receive = {
    case Request(ReleaseCapability, id, params: CapabilityRegistration) =>
      capabilityRouter ! CapabilityProtocol.ReleaseCapability(client.id, params)
      context.system.scheduler.scheduleOnce(timeout, self, RequestTimeout)
      context.become(responseStage(id, sender()))
  }
  private def responseStage(id: Id, replyTo: ActorRef): Receive = {
    case RequestTimeout =>
      log.error(s"Releasing capability for ${client.id} timed out")
      replyTo ! ResponseError(Some(id), ServiceError)
      context.stop(self)

    case CapabilityReleased =>
      replyTo ! ResponseResult(ReleaseCapability, id, Unused)
      context.stop(self)

    case CapabilityReleaseBadRequest =>
      replyTo ! ResponseError(Some(id), ServiceError)
      context.stop(self)
  }

  override def unhandled(message: Any): Unit =
    log.warning("Received unknown message: {}", message)

}

object ReleaseCapabilityHandler {

  /**
    * Creates a configuration object used to create a [[ReleaseCapabilityHandler]]
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
      new ReleaseCapabilityHandler(capabilityRouter, requestTimeout, client)
    )

}
