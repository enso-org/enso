package org.enso.languageserver.runtime

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.pattern.pipe
import org.enso.languageserver.capability.CapabilityProtocol.{
  AcquireCapability,
  CapabilityAcquired,
  CapabilityReleased,
  ReleaseCapability
}
import org.enso.languageserver.data.{
  CapabilityRegistration,
  ClientId,
  Config,
  ReceivesSuggestionsDatabaseUpdates
}
import org.enso.languageserver.filemanager.{FileDeletedEvent, Path}
import org.enso.languageserver.refactoring.ProjectNameChangedEvent
import org.enso.languageserver.runtime.SearchProtocol._
import org.enso.languageserver.session.SessionRouter.DeliverToJsonController
import org.enso.languageserver.util.UnhandledLogging
import org.enso.pkg.PackageManager
import org.enso.polyglot.Suggestion
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.searcher.SuggestionsRepo
import org.enso.text.editing.model.Position

import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
  * The handler of search requests.
  *
  * Handler initializes the database and responds to the search requests.
  *
  * == Implementation ==
  *
  * {{
  *
  *                               +--------------------+
  *                               | SuggestionsRepo    |
  *                               +---------+----------+
  *                                         ^
  *              Capability,Search          |
  *              Request/Response           v
  *  +---------+<---------------->+---------+----------+
  *  | Clients |                  | SuggestionsHandler |
  *  +---------+<-----------------+---------+----------+
  *              Database Update            ^
  *              Notifications              |
  *                                         |
  *                               +---------+----------+
  *                               | RuntimeConnector   |
  *                               +----+----------+----+
  *                                    ^          ^
  *          SuggestionsDatabaseUpdate |          | ExpressionValuesComputed
  *                                    |          |
  *                   +----------------+--+    +--+--------------------+
  *                   | EnsureCompiledJob |    | IdExecutionInstrument |
  *                   +-------------------+    +-----------------------+
  *
  * }}
  *
  * @param config the server configuration
  * @param repo the suggestions repo
  * @param sessionRouter the session router
  */
final class SuggestionsHandler(
  config: Config,
  repo: SuggestionsRepo[Future],
  sessionRouter: ActorRef
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
    context.system.eventStream.subscribe(self, classOf[ProjectNameChangedEvent])
    context.system.eventStream.subscribe(self, classOf[FileDeletedEvent])

    config.contentRoots.foreach {
      case (_, contentRoot) =>
        PackageManager.Default
          .fromDirectory(contentRoot)
          .foreach(pkg => self ! ProjectNameChangedEvent(pkg.config.name))
    }
  }

  override def receive: Receive = {
    case ProjectNameChangedEvent(name) =>
      context.become(initialized(name, Set()))

    case msg =>
      log.warning("Unhandled message: {}", msg)
      sender() ! ProjectNotFoundError
  }

  def initialized(projectName: String, clients: Set[ClientId]): Receive = {
    case AcquireCapability(
          client,
          CapabilityRegistration(ReceivesSuggestionsDatabaseUpdates())
        ) =>
      sender() ! CapabilityAcquired
      context.become(initialized(projectName, clients + client.clientId))

    case ReleaseCapability(
          client,
          CapabilityRegistration(ReceivesSuggestionsDatabaseUpdates())
        ) =>
      sender() ! CapabilityReleased
      context.become(initialized(projectName, clients - client.clientId))

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
            SuggestionsDatabaseUpdateNotification(version, updates)
        }

    case GetSuggestionsDatabaseVersion =>
      repo.currentVersion
        .map(GetSuggestionsDatabaseVersionResult)
        .pipeTo(sender())

    case GetSuggestionsDatabase =>
      repo.getAll
        .map {
          case (version, entries) =>
            GetSuggestionsDatabaseResult(
              version,
              entries.map(SuggestionDatabaseEntry(_))
            )
        }
        .pipeTo(sender())

    case Completion(path, pos, selfType, returnType, tags) =>
      getModule(projectName, path)
        .fold(
          Future.successful,
          module =>
            repo
              .search(
                Some(module),
                selfType,
                returnType,
                tags.map(_.map(SuggestionKind.toSuggestion)),
                Some(toPosition(pos))
              )
              .map(CompletionResult.tupled)
        )
        .pipeTo(sender())

    case FileDeletedEvent(path) =>
      getModule(projectName, path)
        .fold(
          err => Future.successful(Left(err)),
          module =>
            repo
              .removeByModule(module)
              .map {
                case (version, ids) =>
                  Right(
                    SuggestionsDatabaseUpdateNotification(
                      version,
                      ids.map(SuggestionsDatabaseUpdate.Remove)
                    )
                  )
              }
        )
        .onComplete {
          case Success(Right(notification)) =>
            if (notification.updates.nonEmpty) {
              clients.foreach { clientId =>
                sessionRouter ! DeliverToJsonController(clientId, notification)
              }
            }
          case Success(Left(err)) =>
            log.error(
              "Error cleaning the index after file delete event: {}",
              err
            )
          case Failure(ex) =>
            log.error(
              ex,
              "Error cleaning the index after file delete event"
            )
        }

    case ProjectNameChangedEvent(name) =>
      context.become(initialized(name, clients))
  }

  /**
    * Handle the suggestions database re-index update.
    *
    * Function clears existing module suggestions from the database, inserts new
    * suggestions and builds the notification containing combined removed and
    * added suggestions.
    *
    * @param moduleName the module name
    * @param updates the list of updates after the full module re-index
    * @return the API suggestions database update notification
    */
  private def applyReIndexUpdates(
    moduleName: String,
    updates: Seq[Api.SuggestionsDatabaseUpdate.Add]
  ): Future[SuggestionsDatabaseUpdateNotification] = {
    val added = updates.map(_.suggestion)
    for {
      (_, removedIds)     <- repo.removeByModule(moduleName)
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
        version,
        updatesRemoved :++ updatesAdded
      )
    }
  }

  /**
    * Handle the suggestions database update.
    *
    * Function applies notification updates on the suggestions database and
    * builds the notification to the user
    *
    * @param msg the suggestions database update notification from runtime
    * @return the API suggestions database update notification
    */
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
        version,
        updatesRemoved :++ updatesAdded
      )
    }
  }

  /**
    * Build the module name from the requested file path.
    *
    * @param projectName the project name
    * @param path the requested file path
    * @return the module name
    */
  private def getModule(
    projectName: String,
    path: Path
  ): Either[SearchFailure, String] =
    for {
      rootFile <- config.findContentRoot(path.rootId).left.map(FileSystemError)
      module <-
        ModuleNameBuilder
          .build(projectName, rootFile.toPath, path.toFile(rootFile).toPath)
          .toRight(ModuleNameNotResolvedError(path))
    } yield module

  private def toPosition(pos: Position): Suggestion.Position =
    Suggestion.Position(pos.line, pos.character)
}

object SuggestionsHandler {

  /**
    * Creates a configuration object used to create a [[SuggestionsHandler]].
    *
    * @param config the server configuration
    * @param repo the suggestions repo
    * @param sessionRouter the session router
    */
  def props(
    config: Config,
    repo: SuggestionsRepo[Future],
    sessionRouter: ActorRef
  ): Props =
    Props(new SuggestionsHandler(config, repo, sessionRouter))

}
