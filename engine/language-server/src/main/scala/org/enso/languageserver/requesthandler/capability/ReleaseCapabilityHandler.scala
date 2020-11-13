package org.enso.languageserver.requesthandler.capability

import akka.actor.{Actor, ActorLogging, ActorRef, Cancellable, Props}
import org.enso.jsonrpc.Errors.ServiceError
import org.enso.jsonrpc._
import org.enso.languageserver.capability.CapabilityApi.{
  CapabilityNotAcquired,
  ReleaseCapability
}
import org.enso.languageserver.capability.CapabilityProtocol
import org.enso.languageserver.capability.CapabilityProtocol.{
  CapabilityNotAcquiredResponse,
  CapabilityReleaseBadRequest,
  CapabilityReleased
}
import org.enso.languageserver.data.CapabilityRegistration
import org.enso.languageserver.requesthandler.RequestTimeout
import org.enso.languageserver.session.JsonSession
import org.enso.languageserver.util.UnhandledLogging

import scala.concurrent.duration.FiniteDuration

/**
  * A request handler for `capability/release` commands.
  *
  * @param capabilityRouter a router that dispatches capability requests
  * @param timeout a request timeout
  * @param session an object representing a client connected to the language server
  */
class ReleaseCapabilityHandler(
  capabilityRouter: ActorRef,
  timeout: FiniteDuration,
  session: JsonSession
) extends Actor
    with ActorLogging
    with UnhandledLogging {

  override def receive: Receive = requestStage

  import context.dispatcher

  private def requestStage: Receive = {
    case Request(ReleaseCapability, id, params: CapabilityRegistration) =>
      capabilityRouter ! CapabilityProtocol.ReleaseCapability(session, params)
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
      log.error(s"Releasing capability for ${session.clientId} timed out")
      replyTo ! ResponseError(Some(id), ServiceError)
      context.stop(self)

    case CapabilityReleased =>
      replyTo ! ResponseResult(ReleaseCapability, id, Unused)
      cancellable.cancel()
      context.stop(self)

    case CapabilityReleaseBadRequest =>
      replyTo ! ResponseError(Some(id), ServiceError)
      cancellable.cancel()
      context.stop(self)

    case CapabilityNotAcquiredResponse =>
      replyTo ! ResponseError(Some(id), CapabilityNotAcquired)
      cancellable.cancel()
      context.stop(self)
  }
}

object ReleaseCapabilityHandler {

  /**
    * Creates a configuration object used to create a [[ReleaseCapabilityHandler]]
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
      new ReleaseCapabilityHandler(capabilityRouter, requestTimeout, rpcSession)
    )

}
