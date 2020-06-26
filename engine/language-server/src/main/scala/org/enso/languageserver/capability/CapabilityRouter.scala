package org.enso.languageserver.capability

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import org.enso.languageserver.capability.CapabilityProtocol.{
  AcquireCapability,
  ReleaseCapability
}
import org.enso.languageserver.data.{
  CanEdit,
  CapabilityRegistration,
  ReceivesSuggestionsDatabaseUpdates,
  ReceivesTreeUpdates
}
import org.enso.languageserver.monitoring.MonitoringProtocol.{Ping, Pong}
import org.enso.languageserver.util.UnhandledLogging

/**
  * A content based router that routes each capability request to the
  * correct recipient based on the capability object.
  *
  * @param bufferRegistry the recipient of buffer capability requests
  * @param receivesTreeUpdatesHandler the recipient of
  * `receivesTreeUpdates` capability requests
  * @param suggestionsDatabaseEventsListener the recipient of
  * `receivesSuggestionsDatabaseUpdates` capability requests
  */
class CapabilityRouter(
  bufferRegistry: ActorRef,
  receivesTreeUpdatesHandler: ActorRef,
  suggestionsDatabaseEventsListener: ActorRef
) extends Actor
    with ActorLogging
    with UnhandledLogging {

  override def receive: Receive = {
    case Ping =>
      sender() ! Pong

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

    case msg @ AcquireCapability(
          _,
          CapabilityRegistration(ReceivesSuggestionsDatabaseUpdates())
        ) =>
      suggestionsDatabaseEventsListener.forward(msg)

    case msg @ ReleaseCapability(
          _,
          CapabilityRegistration(ReceivesSuggestionsDatabaseUpdates())
        ) =>
      suggestionsDatabaseEventsListener.forward(msg)
  }

}

object CapabilityRouter {

  /**
    * Creates a configuration object used to create a [[CapabilityRouter]]
    *
    * @param bufferRegistry a buffer registry ref
    * @param receivesTreeUpdatesHandler the recipient of `receivesTreeUpdates`
    * capability requests
    * @param suggestionsDatabaseEventsListener the recipient of
    * `receivesSuggestionsDatabaseUpdates` capability requests
    * @return a configuration object
    */
  def props(
    bufferRegistry: ActorRef,
    receivesTreeUpdatesHandler: ActorRef,
    suggestionsDatabaseEventsListener: ActorRef
  ): Props =
    Props(
      new CapabilityRouter(
        bufferRegistry,
        receivesTreeUpdatesHandler,
        suggestionsDatabaseEventsListener
      )
    )

}
