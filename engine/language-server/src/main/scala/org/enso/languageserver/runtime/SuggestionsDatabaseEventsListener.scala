package org.enso.languageserver.runtime

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import org.enso.languageserver.capability.CapabilityProtocol.{
  AcquireCapability,
  CapabilityAcquired,
  CapabilityReleased,
  ReleaseCapability
}
import org.enso.languageserver.data.{
  CapabilityRegistration,
  ClientId,
  ReceivesSuggestionsDatabaseUpdates
}
import org.enso.languageserver.runtime.SearchProtocol.{
  SuggestionsDatabaseUpdate,
  SuggestionsDatabaseUpdateNotification
}
import org.enso.languageserver.session.SessionRouter.DeliverToJsonController
import org.enso.languageserver.util.UnhandledLogging
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.searcher.{Suggestion, SuggestionsRepo}

import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
  * Event listener listens event stream for the suggestion database
  * notifications from the runtime and sends updates to the client. The listener
  * is a singleton and created per context registry.
  *
  * @param sessionRouter the session router
  * @param repo the suggestions repo
  */
final class SuggestionsDatabaseEventsListener(
  sessionRouter: ActorRef,
  repo: SuggestionsRepo[Future]
) extends Actor
    with ActorLogging
    with UnhandledLogging {

  import context.dispatcher

  override def preStart(): Unit = {
    context.system.eventStream
      .subscribe(self, classOf[Api.SuggestionsDatabaseUpdateNotification])
  }

  override def receive: Receive = withClients(Set())

  private def withClients(clients: Set[ClientId]): Receive = {
    case AcquireCapability(
          client,
          CapabilityRegistration(ReceivesSuggestionsDatabaseUpdates())
        ) =>
      sender() ! CapabilityAcquired
      context.become(withClients(clients + client.clientId))

    case ReleaseCapability(
          client,
          CapabilityRegistration(ReceivesSuggestionsDatabaseUpdates())
        ) =>
      sender() ! CapabilityReleased
      context.become(withClients(clients - client.clientId))

    case msg: Api.SuggestionsDatabaseUpdateNotification =>
      applyDatabaseUpdates(msg)
        .onComplete {
          case Success(notification) =>
            if (notification.updates.nonEmpty) {
              clients.foreach { clientId =>
                sessionRouter ! DeliverToJsonController(clientId, notification)
              }
            }
          case Failure(ex) =>
            log.error(
              ex,
              "Error applying suggestion database updates: {}",
              msg.updates
            )
        }
  }

  private def applyDatabaseUpdates(
    msg: Api.SuggestionsDatabaseUpdateNotification
  ): Future[SuggestionsDatabaseUpdateNotification] = {
    val (added, removed) = msg.updates
      .foldLeft((Seq[Suggestion](), Seq[Suggestion]())) {
        case ((add, remove), msg: Api.SuggestionsDatabaseUpdate.Add) =>
          (add :+ msg.suggestion, remove)
        case ((add, remove), msg: Api.SuggestionsDatabaseUpdate.Remove) =>
          (add, remove :+ msg.suggestion)
      }

    for {
      (_, removedIds)     <- repo.removeAll(removed)
      (version, addedIds) <- repo.insertAll(added)
    } yield {
      val updatesRemoved = removedIds.collect {
        case Some(id) => SuggestionsDatabaseUpdate.Remove(id)
      }
      val updatesAdded =
        (addedIds zip added).flatMap {
          case (Some(id), suggestion) =>
            Some(SuggestionsDatabaseUpdate.Add(id, suggestion))
          case (None, suggestion) =>
            log.error("failed to insert suggestion: {}", suggestion)
            None
        }
      SuggestionsDatabaseUpdateNotification(
        updatesRemoved ++ updatesAdded,
        version
      )
    }
  }
}

object SuggestionsDatabaseEventsListener {

  /**
    * Creates a configuration object used to create a
    * [[SuggestionsDatabaseEventsListener]].
    *
    * @param sessionRouter the session router
    * @param repo the suggestions repo
    */
  def props(
    sessionRouter: ActorRef,
    repo: SuggestionsRepo[Future]
  ): Props =
    Props(new SuggestionsDatabaseEventsListener(sessionRouter, repo))

}
