package org.enso.languageserver.runtime

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import org.enso.languageserver.capability.CapabilityProtocol.{
  AcquireCapability,
  ReleaseCapability
}
import org.enso.languageserver.data.{
  CapabilityRegistration,
  ClientId,
  ReceivesSuggestionsDatabaseUpdates
}
import org.enso.languageserver.runtime.SuggestionsDatabaseEventsApi.SuggestionsDatabaseUpdate
import org.enso.languageserver.session.SessionRouter.DeliverToBinaryController
import org.enso.languageserver.util.UnhandledLogging
import org.enso.polyglot.runtime.Runtime.Api

/**
  * Event listener listens event stream for the suggestion database
  * notifications from the runtime and sends updates to the client. The listener
  * is a singleton and created per context registry.
  *
  * @param sessionRouter the session router
  */
final class SuggestionsDatabaseEventsListener(
  sessionRouter: ActorRef
) extends Actor
    with ActorLogging
    with UnhandledLogging {

  override def preStart(): Unit = {
    context.system.eventStream
      .subscribe(self, classOf[Api.SuggestionsDatabaseUpdate])
  }

  override def receive: Receive = withClients(Set())

  private def withClients(clients: Set[ClientId]): Receive = {
    case AcquireCapability(
          client,
          CapabilityRegistration(ReceivesSuggestionsDatabaseUpdates())
        ) =>
      withClients(clients + client.clientId)

    case ReleaseCapability(
          client,
          CapabilityRegistration(ReceivesSuggestionsDatabaseUpdates())
        ) =>
      withClients(clients - client.clientId)

    case msg: Api.SuggestionsDatabaseUpdate =>
      clients.foreach { clientId =>
        sessionRouter ! DeliverToBinaryController(clientId, toUpdate(msg))
      }
  }

  private def toUpdate(
    update: Api.SuggestionsDatabaseUpdate
  ): SuggestionsDatabaseUpdate =
    update match {
      case Api.SuggestionsDatabaseUpdate.Add(id, suggestion) =>
        SuggestionsDatabaseUpdate.Add(id, suggestion)
      case Api.SuggestionsDatabaseUpdate.Modify(
            id,
            name,
            arguments,
            selfType,
            returnType,
            doc,
            scope
          ) =>
        SuggestionsDatabaseUpdate.Modify(
          id,
          name,
          arguments,
          selfType,
          returnType,
          doc,
          scope
        )
      case Api.SuggestionsDatabaseUpdate.Remove(id) =>
        SuggestionsDatabaseUpdate.Remove(id)
    }
}

object SuggestionsDatabaseEventsListener {

  /**
    * Creates a configuration object used to create a
    * [[SuggestionsDatabaseEventsListener]].
    *
    * @param sessionRouter the session router
    */
  def props(sessionRouter: ActorRef): Props =
    Props(new SuggestionsDatabaseEventsListener(sessionRouter))

}
