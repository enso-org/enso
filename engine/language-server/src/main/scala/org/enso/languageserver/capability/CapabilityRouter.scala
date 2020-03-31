package org.enso.languageserver.capability

import akka.actor.{Actor, ActorRef, Props}
import org.enso.languageserver.capability.CapabilityProtocol.{
  AcquireCapability,
  ReleaseCapability
}
import org.enso.languageserver.data.{
  CanEdit,
  CapabilityRegistration,
  ReceivesTreeUpdates
}

/**
  * A content based router that routes each capability request to the
  * correct recipient based on the capability object.
  *
  * @param bufferRegistry the recipient of buffer capability requests
  * @param receivesTreeUpdatesHandler the recipient of
  * `receivesTreeUpdates` capability requests
  */
class CapabilityRouter(
  bufferRegistry: ActorRef,
  receivesTreeUpdatesHandler: ActorRef
) extends Actor {

  override def receive: Receive = {
    case msg @ AcquireCapability(_, CapabilityRegistration(CanEdit(_))) =>
      bufferRegistry.forward(msg)

    case msg @ ReleaseCapability(_, CapabilityRegistration(CanEdit(_))) =>
      bufferRegistry.forward(msg)

    case msg @ AcquireCapability(
          _,
          CapabilityRegistration(ReceivesTreeUpdates(_))
        ) =>
      receivesTreeUpdatesHandler.forward(msg)

    case msg @ ReleaseCapability(
          _,
          CapabilityRegistration(ReceivesTreeUpdates(_))
        ) =>
      receivesTreeUpdatesHandler.forward(msg)
  }

}

object CapabilityRouter {

  /**
    * Creates a configuration object used to create a [[CapabilityRouter]]
    *
    * @param bufferRegistry a buffer registry ref
    * @return a configuration object
    */
  def props(
    bufferRegistry: ActorRef,
    receivesTreeUpdatesHandler: ActorRef
  ): Props =
    Props(new CapabilityRouter(bufferRegistry, receivesTreeUpdatesHandler))

}
