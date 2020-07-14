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
import org.enso.polyglot.Suggestion
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.searcher.SuggestionsRepo

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
      .subscribe(self, classOf[Api.ExpressionValuesComputed])
    context.system.eventStream
      .subscribe(self, classOf[Api.SuggestionsDatabaseUpdateNotification])
    context.system.eventStream
      .subscribe(self, classOf[Api.SuggestionsDatabaseReIndexNotification])
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

    case msg: Api.SuggestionsDatabaseReIndexNotification =>
      applyReIndexUpdates(msg.moduleName, msg.updates)
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
              "Error applying suggestion re-index updates: {}",
              msg.updates
            )
        }

    case Api.ExpressionValuesComputed(_, updates) =>
      val types = updates.flatMap(update =>
        update.expressionType.map(update.expressionId -> _)
      )
      repo
        .updateAll(types)
        .map {
          case (version, updatedIds) =>
            val updates = types.zip(updatedIds).collect {
              case ((_, typeValue), Some(suggestionId)) =>
                SuggestionsDatabaseUpdate.Modify(suggestionId, typeValue)
            }
            SuggestionsDatabaseUpdateNotification(updates, version)
        }
  }

  private def applyReIndexUpdates(
    moduleName: String,
    updates: Seq[Api.SuggestionsDatabaseUpdate.Add]
  ): Future[SuggestionsDatabaseUpdateNotification] = {
    val added = updates.map(_.suggestion)
    for {
      removedIds          <- repo.removeByModule(moduleName)
      (version, addedIds) <- repo.insertAll(added)
    } yield {
      val updatesRemoved = removedIds.map(SuggestionsDatabaseUpdate.Remove)
      val updatesAdded = (addedIds zip added).flatMap {
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
