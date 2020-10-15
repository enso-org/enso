package org.enso.languageserver.search

import java.util

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Stash}
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
  ContentBasedVersioning,
  ReceivesSuggestionsDatabaseUpdates
}
import org.enso.languageserver.event.InitializedEvent
import org.enso.languageserver.filemanager.{FileDeletedEvent, Path}
import org.enso.languageserver.refactoring.ProjectNameChangedEvent
import org.enso.languageserver.search.SearchProtocol._
import org.enso.languageserver.search.handler.InvalidateModulesIndexHandler
import org.enso.languageserver.session.SessionRouter.DeliverToJsonController
import org.enso.languageserver.util.UnhandledLogging
import org.enso.pkg.PackageManager
import org.enso.polyglot.Suggestion
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.searcher.{FileVersionsRepo, SuggestionsRepo}
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
  * @param suggestionsRepo the suggestions repo
  * @param sessionRouter the session router
  * @param runtimeConnector the runtime connector
  */
final class SuggestionsHandler(
  config: Config,
  suggestionsRepo: SuggestionsRepo[Future],
  fileVersionsRepo: FileVersionsRepo[Future],
  sessionRouter: ActorRef,
  runtimeConnector: ActorRef
)(implicit versionCalculator: ContentBasedVersioning)
    extends Actor
    with Stash
    with ActorLogging
    with UnhandledLogging {

  import SuggestionsHandler.ProjectNameUpdated
  import context.dispatcher

  private val timeout = config.executionContext.requestTimeout

  override def preStart(): Unit = {
    context.system.eventStream
      .subscribe(self, classOf[Api.ExpressionValuesComputed])
    context.system.eventStream
      .subscribe(self, classOf[Api.SuggestionsDatabaseModuleUpdateNotification])
    context.system.eventStream.subscribe(self, classOf[ProjectNameChangedEvent])
    context.system.eventStream.subscribe(self, classOf[FileDeletedEvent])
    context.system.eventStream
      .subscribe(self, InitializedEvent.SuggestionsRepoInitialized.getClass)

    config.contentRoots.foreach {
      case (_, contentRoot) =>
        PackageManager.Default
          .fromDirectory(contentRoot)
          .foreach(pkg => self ! ProjectNameUpdated(pkg.config.name))
    }
  }

  override def receive: Receive =
    initializing(SuggestionsHandler.Initialization())

  def initializing(init: SuggestionsHandler.Initialization): Receive = {
    case ProjectNameChangedEvent(oldName, newName) =>
      suggestionsRepo
        .renameProject(oldName, newName)
        .map(_ => ProjectNameUpdated(newName))
        .pipeTo(self)
    case ProjectNameUpdated(name) =>
      tryInitialize(init.copy(project = Some(name)))
    case InitializedEvent.SuggestionsRepoInitialized =>
      tryInitialize(
        init.copy(suggestions =
          Some(InitializedEvent.SuggestionsRepoInitialized)
        )
      )
    case _ => stash()
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

    case msg: Api.SuggestionsDatabaseModuleUpdateNotification =>
      val isVersionChanged =
        fileVersionsRepo.getVersion(msg.file).map { versionOpt =>
          versionOpt.fold(true)(
            !util.Arrays.equals(_, versionCalculator.evalDigest(msg.contents))
          )
        }
      val applyUpdatesIfVersionChanged =
        isVersionChanged.flatMap { isChanged =>
          if (isChanged) applyDatabaseUpdates(msg).map(Some(_))
          else Future.successful(None)
        }
      applyUpdatesIfVersionChanged
        .onComplete {
          case Success(Some(notification)) =>
            if (notification.updates.nonEmpty) {
              clients.foreach { clientId =>
                sessionRouter ! DeliverToJsonController(clientId, notification)
              }
            }
          case Success(None) =>
          case Failure(ex) =>
            log.error(
              ex,
              "Error applying suggestion database updates: {}",
              msg.file
            )
        }

    case Api.ExpressionValuesComputed(_, updates) =>
      log.debug(
        s"ExpressionValuesComputed ${updates.map(u => (u.expressionId, u.expressionType))}"
      )
      val types = updates
        .flatMap(update => update.expressionType.map(update.expressionId -> _))
      suggestionsRepo
        .updateAll(types)
        .map {
          case (version, updatedIds) =>
            val updates = types.zip(updatedIds).collect {
              case ((_, typeValue), Some(suggestionId)) =>
                SuggestionsDatabaseUpdate.Modify(suggestionId, typeValue)
            }
            SuggestionsDatabaseUpdateNotification(version, updates)
        }
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
              "Error applying changes from computed values: {}",
              updates
            )
        }

    case GetSuggestionsDatabaseVersion =>
      suggestionsRepo.currentVersion
        .map(GetSuggestionsDatabaseVersionResult)
        .pipeTo(sender())

    case GetSuggestionsDatabase =>
      suggestionsRepo.getAll
        .map {
          case (version, entries) =>
            GetSuggestionsDatabaseResult(
              version,
              entries.map(SuggestionDatabaseEntry(_))
            )
        }
        .pipeTo(sender())

    case Completion(path, pos, selfType, returnType, tags) =>
      getModuleName(projectName, path)
        .fold(
          Future.successful,
          module =>
            suggestionsRepo
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
      getModuleName(projectName, path)
        .fold(
          err => Future.successful(Left(err)),
          module =>
            suggestionsRepo
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

    case InvalidateSuggestionsDatabase =>
      val action = for {
        _ <- suggestionsRepo.clean
        _ <- fileVersionsRepo.clean
      } yield SearchProtocol.InvalidateModulesIndex

      val handler = context.system
        .actorOf(InvalidateModulesIndexHandler.props(timeout, runtimeConnector))
      action.pipeTo(handler)(sender())

    case ProjectNameChangedEvent(oldName, newName) =>
      suggestionsRepo
        .renameProject(oldName, newName)
        .map(_ => ProjectNameUpdated(newName))
        .pipeTo(self)

    case ProjectNameUpdated(name) =>
      context.become(initialized(name, clients))
  }

  /**
    * Transition the initialization process.
    *
    * @param state current initialization state
    */
  private def tryInitialize(state: SuggestionsHandler.Initialization): Unit = {
    state.initialized.fold(context.become(initializing(state))) { name =>
      log.debug("Initialized")
      context.become(initialized(name, Set()))
      unstashAll()
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
    msg: Api.SuggestionsDatabaseModuleUpdateNotification
  ): Future[SuggestionsDatabaseUpdateNotification] = {
    val (addCmds, removeCmds, cleanCmds) = msg.updates
      .foldLeft(
        (Vector[Suggestion](), Vector[Suggestion](), Vector[String]())
      ) {
        case ((add, remove, clean), m: Api.SuggestionsDatabaseUpdate.Add) =>
          (add :+ m.suggestion, remove, clean)
        case ((add, remove, clean), m: Api.SuggestionsDatabaseUpdate.Remove) =>
          (add, remove :+ m.suggestion, clean)
        case ((add, remove, clean), m: Api.SuggestionsDatabaseUpdate.Clean) =>
          (add, remove, clean :+ m.module)
      }
    val fileVersion = versionCalculator.evalDigest(msg.contents)
    log.debug(
      s"Applying suggestion updates: Add(${addCmds.map(_.name).mkString(",")}); Remove(${removeCmds
        .map(_.name)
        .mkString(",")}); Clean(${cleanCmds.mkString(",")})"
    )
    for {
      (_, cleanedIds)     <- suggestionsRepo.removeAllByModule(cleanCmds)
      (_, removedIds)     <- suggestionsRepo.removeAll(removeCmds)
      (version, addedIds) <- suggestionsRepo.insertAll(addCmds)
      _                   <- fileVersionsRepo.setVersion(msg.file, fileVersion)
    } yield {
      val updatesCleaned = cleanedIds.map(SuggestionsDatabaseUpdate.Remove)
      val updatesRemoved =
        (removedIds zip removeCmds).flatMap {
          case (Some(id), _) =>
            Some(SuggestionsDatabaseUpdate.Remove(id))
          case (None, suggestion) =>
            log.error("Failed to remove suggestion: {}", suggestion)
            None
        }
      val updatesAdded =
        (addedIds zip addCmds).flatMap {
          case (Some(id), suggestion) =>
            Some(SuggestionsDatabaseUpdate.Add(id, suggestion))
          case (None, suggestion) =>
            log.error("Failed to insert suggestion: {}", suggestion)
            None
        }
      SuggestionsDatabaseUpdateNotification(
        version,
        updatesCleaned :++ updatesRemoved :++ updatesAdded
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
  private def getModuleName(
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
    * The notification about the project name update.
    *
    * @param projectName the new project name
    */
  case class ProjectNameUpdated(projectName: String)

  /**
    * The initialization state of the handler.
    *
    * @param project the project name
    * @param suggestions the initialization event of the suggestions repo
    */
  private case class Initialization(
    project: Option[String]                                               = None,
    suggestions: Option[InitializedEvent.SuggestionsRepoInitialized.type] = None
  ) {

    /**
      * Check if all the components are initialized.
      *
      * @return the project name
      */
    def initialized: Option[String] =
      for {
        _    <- suggestions
        name <- project
      } yield name
  }

  /**
    * Creates a configuration object used to create a [[SuggestionsHandler]].
    *
    * @param config the server configuration
    * @param suggestionsRepo the suggestions repo
    * @param fileVersionsRepo the file versions repo
    * @param sessionRouter the session router
    * @param runtimeConnector the runtime connector
    */
  def props(
    config: Config,
    suggestionsRepo: SuggestionsRepo[Future],
    fileVersionsRepo: FileVersionsRepo[Future],
    sessionRouter: ActorRef,
    runtimeConnector: ActorRef
  )(implicit versionCalculator: ContentBasedVersioning): Props =
    Props(
      new SuggestionsHandler(
        config,
        suggestionsRepo,
        fileVersionsRepo,
        sessionRouter,
        runtimeConnector
      )
    )

}
