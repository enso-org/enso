package org.enso.languageserver.search

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
      .subscribe(self, classOf[Api.SuggestionsDatabaseUpdateNotification])
    context.system.eventStream
      .subscribe(self, classOf[Api.SuggestionsDatabaseReIndexNotification])
    context.system.eventStream
      .subscribe(self, classOf[Api.SuggestionsDatabaseIndexUpdateNotification])
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

    case msg: Api.SuggestionsDatabaseIndexUpdateNotification =>
      applyIndexedModuleUpdates(msg.updates.toSeq)
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
              msg.updates.map(_.file)
            )
        }

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
      log.debug(s"ReIndex ${msg.moduleName} ${msg.updates.map(_.suggestion)}")
      applyReIndexUpdates(msg.updates)
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

  private def applyIndexedModuleUpdates(
    updates: Seq[Api.IndexedModule]
  ): Future[SuggestionsDatabaseUpdateNotification] = {
    def createIndexedModuleUpdatesBatch(
      contents: String,
      file: java.io.File,
      updates: Seq[Api.SuggestionsDatabaseUpdate.Add]
    ): Future[Seq[Api.SuggestionsDatabaseUpdate.Add]] =
      fileVersionsRepo
        .updateVersion(file, versionCalculator.evalDigest(contents))
        .map(versionChanged => if (versionChanged) updates else Seq())
    def getBatches =
      Future
        .traverse(updates) { indexed =>
          createIndexedModuleUpdatesBatch(
            indexed.contents,
            indexed.file,
            indexed.updates
          )
        }
        .map(_.flatten)
    for {
      batch  <- getBatches
      update <- applyReIndexUpdates(batch)
    } yield update
  }

  /**
    * Handle the suggestions database re-index update.
    *
    * Function clears existing module suggestions from the database, inserts new
    * suggestions and builds the notification containing combined removed and
    * added suggestions.
    *
    * @param updates the list of updates after the full module re-index
    * @return the API suggestions database update notification
    */
  private def applyReIndexUpdates(
    updates: Seq[Api.SuggestionsDatabaseUpdate.Add]
  ): Future[SuggestionsDatabaseUpdateNotification] = {
    val added   = updates.map(_.suggestion)
    val modules = updates.map(_.suggestion.module).distinct
    log.debug(s"Applying re-index updates; modules=$modules")
    for {
      (_, removedIds)     <- suggestionsRepo.removeAllByModule(modules)
      (version, addedIds) <- suggestionsRepo.insertAll(added)
    } yield {
      val updatesRemoved = removedIds.map(SuggestionsDatabaseUpdate.Remove)
      val updatesAdded = (addedIds zip added).flatMap {
        case (Some(id), suggestion) =>
          Some(SuggestionsDatabaseUpdate.Add(id, suggestion))
        case (None, suggestion) =>
          log.error("Failed to insert re-index suggestion: {}", suggestion)
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
    log.debug(
      s"Applying suggestion updates; added=${added
        .map(_.name)}; removed=${removed.map(_.name)}"
    )
    for {
      (_, removedIds)     <- suggestionsRepo.removeAll(removed)
      (version, addedIds) <- suggestionsRepo.insertAll(added)
    } yield {
      val updatesRemoved = removedIds.collect {
        case Some(id) => SuggestionsDatabaseUpdate.Remove(id)
      }
      val updatesAdded =
        (addedIds zip added).flatMap {
          case (Some(id), suggestion) =>
            Some(SuggestionsDatabaseUpdate.Add(id, suggestion))
          case (None, suggestion) =>
            log.error("Failed to insert suggestion: {}", suggestion)
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
