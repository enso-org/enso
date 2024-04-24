package org.enso.languageserver.protocol.json

import akka.actor.{Actor, ActorRef, Cancellable, Props, Stash, Status}
import akka.pattern.pipeCompletionStage
import akka.util.Timeout
import com.typesafe.scalalogging.LazyLogging
import org.enso.cli.task.ProgressUnit
import org.enso.cli.task.notifications.TaskNotificationApi
import org.enso.jsonrpc._
import org.enso.languageserver.ai.AICompletion
import org.enso.languageserver.boot.resource.{
  InitializationComponent,
  InitializationComponentInitialized
}
import org.enso.languageserver.capability.CapabilityApi.{
  AcquireCapability,
  ForceReleaseCapability,
  GrantCapability,
  ReleaseCapability
}
import org.enso.languageserver.capability.CapabilityProtocol
import org.enso.languageserver.data.Config
import org.enso.languageserver.event.{
  InitializedEvent,
  JsonSessionInitialized,
  JsonSessionTerminated
}
import org.enso.languageserver.filemanager.FileManagerApi._
import org.enso.languageserver.filemanager._
import org.enso.languageserver.io.InputOutputApi._
import org.enso.languageserver.io.OutputKind.{StandardError, StandardOutput}
import org.enso.languageserver.io.{InputOutputApi, InputOutputProtocol}
import org.enso.languageserver.libraries.LibraryApi._
import org.enso.languageserver.libraries.LibraryConfig
import org.enso.languageserver.libraries.handler._
import org.enso.languageserver.monitoring.MonitoringApi.{InitialPing, Ping}
import org.enso.languageserver.monitoring.MonitoringProtocol
import org.enso.languageserver.profiling.ProfilingApi.{
  ProfilingSnapshot,
  ProfilingStart,
  ProfilingStop
}
import org.enso.languageserver.refactoring.RefactoringApi.{
  RenameProject,
  RenameSymbol
}
import org.enso.languageserver.refactoring.{RefactoringApi, RefactoringProtocol}
import org.enso.languageserver.requesthandler._
import org.enso.languageserver.requesthandler.capability._
import org.enso.languageserver.requesthandler.io._
import org.enso.languageserver.requesthandler.monitoring.{
  InitialPingHandler,
  PingHandler
}
import org.enso.languageserver.requesthandler.profiling.{
  ProfilingSnapshotHandler,
  ProfilingStartHandler,
  ProfilingStopHandler
}
import org.enso.languageserver.requesthandler.refactoring.{
  RenameProjectHandler,
  RenameSymbolHandler
}
import org.enso.languageserver.requesthandler.text._
import org.enso.languageserver.requesthandler.visualization.{
  AttachVisualizationHandler,
  DetachVisualizationHandler,
  ExecuteExpressionHandler,
  ModifyVisualizationHandler
}
import org.enso.languageserver.requesthandler.workspace.ProjectInfoHandler
import org.enso.languageserver.runtime.ContextRegistryProtocol
import org.enso.languageserver.runtime.ExecutionApi._
import org.enso.languageserver.runtime.RuntimeApi.RuntimeGetComponentGroups
import org.enso.languageserver.runtime.VisualizationApi.{
  AttachVisualization,
  DetachVisualization,
  ExecuteExpression,
  ModifyVisualization
}
import org.enso.languageserver.search.SearchApi._
import org.enso.languageserver.search.{SearchApi, SearchProtocol}
import org.enso.languageserver.session.JsonSession
import org.enso.languageserver.session.SessionApi.{
  InitProtocolConnection,
  ResourcesInitializationError,
  SessionAlreadyInitialisedError,
  SessionNotInitialisedError
}
import org.enso.languageserver.text.TextApi._
import org.enso.languageserver.text.TextProtocol
import org.enso.languageserver.util.UnhandledLogging
import org.enso.languageserver.vcsmanager.VcsManagerApi._
import org.enso.languageserver.workspace.WorkspaceApi.ProjectInfo
import org.enso.logger.akka.ActorMessageLogging
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.polyglot.runtime.Runtime.Api.ProgressNotification

import java.util.UUID

import scala.concurrent.duration._

/** An actor handling communications between a single client and the language
  * server.
  *
  * @param connectionId the internal connection id
  * @param mainComponent the main initialization logic
  * @param bufferRegistry a router that dispatches text editing requests
  * @param capabilityRouter a router that dispatches capability requests
  * @param fileManager performs operations with file system
  * @param vcsManager performs operations with VCS
  * @param contentRootManager manages the available content roots
  * @param contextRegistry a router that dispatches execution context requests
  * @param suggestionsHandler a reference to the suggestions requests handler
  * @param runtimeConnector a reference to the runtime connector
  * @param idlenessMonitor a reference to the idleness monitor actor
  * @param projectSettingsManager a reference to the project settings manager
  * @param profilingManager a reference to the profiling manager
  * @param libraryConfig configuration of the library ecosystem
  * @param requestTimeout a request timeout
  */
class JsonConnectionController(
  val connectionId: UUID,
  val mainComponent: InitializationComponent,
  val bufferRegistry: ActorRef,
  val capabilityRouter: ActorRef,
  val fileManager: ActorRef,
  val vcsManager: ActorRef,
  val contentRootManager: ActorRef,
  val contextRegistry: ActorRef,
  val suggestionsHandler: ActorRef,
  val stdOutController: ActorRef,
  val stdErrController: ActorRef,
  val stdInController: ActorRef,
  val runtimeConnector: ActorRef,
  val idlenessMonitor: ActorRef,
  val projectSettingsManager: ActorRef,
  val profilingManager: ActorRef,
  val libraryConfig: LibraryConfig,
  val languageServerConfig: Config,
  requestTimeout: FiniteDuration = 10.seconds
) extends Actor
    with Stash
    with LazyLogging
    with ActorMessageLogging
    with UnhandledLogging {

  import context.dispatcher

  implicit val timeout: Timeout = Timeout(requestTimeout)

  override def preStart(): Unit = {
    super.preStart()

    context.system.eventStream
      .subscribe(self, classOf[RefactoringProtocol.ProjectRenamedNotification])
  }

  override def receive: Receive = LoggingReceive {
    case JsonRpcServer.WebConnect(webActor, _) =>
      unstashAll()
      context.become(connected(webActor))
    case _ => stash()
  }

  private def connected(webActor: ActorRef): Receive = LoggingReceive {
    case req @ Request(Ping, _, Unused) =>
      val handler = context.actorOf(
        PingHandler.props(
          List(
            bufferRegistry,
            capabilityRouter,
            fileManager,
            contextRegistry
          ),
          requestTimeout
        )
      )
      handler.forward(req)

    case req @ Request(RenameProject, _, _) =>
      val handler = context.actorOf(
        RenameProjectHandler.props(requestTimeout, runtimeConnector)
      )
      handler.forward(req)

    case req @ Request(
          InitProtocolConnection,
          _,
          InitProtocolConnection.Params(clientId)
        ) =>
      logger.info(
        "Initializing resources for [{}] [{}].",
        clientId,
        mainComponent
      )
      mainComponent
        .init()
        .thenApply(_ => InitializationComponentInitialized.getInstance)
        .pipeTo(self)
      context.become(initializing(webActor, clientId, req, sender()))

    case Request(_, id, _) =>
      sender() ! ResponseError(Some(id), SessionNotInitialisedError)

    case MessageHandler.Disconnected(_) =>
      context.stop(self)
  }

  private def initializing(
    webActor: ActorRef,
    clientId: UUID,
    request: Request[_, _],
    receiver: ActorRef
  ): Receive = LoggingReceive {
    case _: InitializationComponentInitialized =>
      logger.info("RPC session initialized for client [{}].", clientId)
      val session = JsonSession(clientId, self)
      context.system.eventStream.publish(JsonSessionInitialized(session))
      context.system.eventStream.publish(
        InitializedEvent.InitializationFinished
      )

      val cancellable = context.system.scheduler.scheduleOnce(
        requestTimeout,
        self,
        RequestTimeout
      )

      contentRootManager ! ContentRootManagerProtocol.SubscribeToNotifications

      context.become(
        waitingForContentRoots(
          webActor    = webActor,
          rpcSession  = session,
          request     = request,
          receiver    = receiver,
          cancellable = cancellable,
          rootsSoFar  = Nil
        )
      )
    case Status.Failure(ex) =>
      logger.error("Failed to initialize resources.", ex)
      receiver ! ResponseError(Some(request.id), ResourcesInitializationError)
      context.system.eventStream.publish(InitializedEvent.InitializationFailed)
      context.become(connected(webActor))

    case _ => stash()
  }

  /** Waits for the ContentRootsAddedNotification that will contain the project
    *  root and finalizes the initialization.
    *
    * Normally just after subscription is registered, a first notification is
    * sent containing all available roots (so including the initial root).
    * However, it may so happen that a root is added just right after that and
    * the messages are reordered, making the other notification processed first.
    * However, our first reply to the IDE must contain the main project root, so
    * to ensure that this is the case we check if the roots contain it and if
    * not, wait for another notification (which must happen, because the project
    * root is always present and so must be sent to the subscriber in one of the
    * messages after the subscription is established).
    */
  private def waitingForContentRoots(
    webActor: ActorRef,
    rpcSession: JsonSession,
    request: Request[_, _],
    receiver: ActorRef,
    cancellable: Cancellable,
    rootsSoFar: List[ContentRootWithFile]
  ): Receive = LoggingReceive {
    case ContentRootManagerProtocol.ContentRootsAddedNotification(roots) =>
      val allRoots = roots ++ rootsSoFar
      val hasProject = roots.exists {
        case ContentRootWithFile(ContentRoot.Project(_), _) => true
        case _                                              => false
      }
      if (hasProject) {
        cancellable.cancel()
        unstashAll()

        receiver ! ResponseResult(
          InitProtocolConnection,
          request.id,
          InitProtocolConnection.Result(
            buildinfo.Info.ensoVersion,
            buildinfo.Info.currentEdition,
            allRoots.map(_.toContentRoot).toSet
          )
        )

        initialize(webActor, rpcSession)
      } else {
        context.become(
          waitingForContentRoots(
            webActor    = webActor,
            rpcSession  = rpcSession,
            request     = request,
            receiver    = receiver,
            cancellable = cancellable,
            rootsSoFar  = roots ++ rootsSoFar
          )
        )
      }

    case RequestTimeout =>
      logger.error("Getting content roots request [{}] timed out.", request.id)
      receiver ! ResponseError(Some(request.id), Errors.RequestTimeout)
      context.stop(self)

    case _ =>
      stash()
  }

  private def initialize(
    webActor: ActorRef,
    rpcSession: JsonSession
  ): Unit = {
    val requestHandlers = createRequestHandlers(rpcSession)
    context.become(initialised(webActor, rpcSession, requestHandlers))

    context.system.eventStream
      .subscribe(self, classOf[Api.ProgressNotification])
  }

  private def initialised(
    webActor: ActorRef,
    rpcSession: JsonSession,
    requestHandlers: Map[Method, Props]
  ): Receive = LoggingReceive {
    case Request(InitProtocolConnection, id, _) =>
      sender() ! ResponseError(Some(id), SessionAlreadyInitialisedError)

    case MessageHandler.Disconnected(_) =>
      logger.info("Json session terminated [{}].", rpcSession.clientId)
      context.system.eventStream.publish(JsonSessionTerminated(rpcSession))
      context.stop(self)

    case CapabilityProtocol.CapabilityForceReleased(registration) =>
      webActor ! Notification(ForceReleaseCapability, registration)

    case CapabilityProtocol.CapabilityGranted(registration) =>
      webActor ! Notification(GrantCapability, registration)

    case TextProtocol.TextDidChange(changes) =>
      webActor ! Notification(TextDidChange, TextDidChange.Params(changes))

    case TextProtocol.FileAutoSaved(path) =>
      webActor ! Notification(FileAutoSaved, FileAutoSaved.Params(path))

    case TextProtocol.FileModifiedOnDisk(path) =>
      webActor ! Notification(
        FileModifiedOnDisk,
        FileModifiedOnDisk.Params(path)
      )

    case TextProtocol.FileEvent(path, event, attributes) =>
      webActor ! Notification(
        EventFile,
        EventFile.Params(path, event, attributes)
      )

    case PathWatcherProtocol.FileEventResult(event) =>
      webActor ! Notification(
        EventFile,
        EventFile.Params(event.path, event.kind, event.attributes.toOption)
      )

    case ContextRegistryProtocol
          .ExpressionUpdatesNotification(contextId, updates) =>
      webActor ! Notification(
        ExecutionContextExpressionUpdates,
        ExecutionContextExpressionUpdates.Params(contextId, updates)
      )

    case ContextRegistryProtocol.ExecutionFailedNotification(
          contextId,
          failure
        ) =>
      webActor ! Notification(
        ExecutionContextExecutionFailed,
        ExecutionContextExecutionFailed.Params(
          contextId,
          failure.message,
          failure.path
        )
      )

    case ContextRegistryProtocol.ExecutionCompleteNotification(contextId) =>
      webActor ! Notification(
        ExecutionContextExecutionComplete,
        ExecutionContextExecutionComplete.Params(contextId)
      )

    case ContextRegistryProtocol.ExecutionDiagnosticNotification(
          contextId,
          diagnostics
        ) =>
      webActor ! Notification(
        ExecutionContextExecutionStatus,
        ExecutionContextExecutionStatus.Params(contextId, diagnostics)
      )

    case ContextRegistryProtocol.VisualizationEvaluationFailed(
          ctx,
          message,
          diagnostic
        ) =>
      webActor ! Notification(
        VisualizationEvaluationFailed,
        VisualizationEvaluationFailed.Params(
          ctx.contextId,
          ctx.visualizationId,
          ctx.expressionId,
          message,
          diagnostic
        )
      )

    case SearchProtocol.SuggestionsDatabaseUpdateNotification(
          version,
          updates
        ) =>
      webActor ! Notification(
        SearchApi.SuggestionsDatabaseUpdates,
        SearchApi.SuggestionsDatabaseUpdates.Params(updates, version)
      )

    case InputOutputProtocol.OutputAppended(output, outputKind) =>
      outputKind match {
        case StandardOutput =>
          webActor ! Notification(
            StandardOutputAppended,
            StandardOutputAppended.Params(output)
          )

        case StandardError =>
          webActor ! Notification(
            StandardErrorAppended,
            StandardErrorAppended.Params(output)
          )

      }

    case InputOutputProtocol.WaitingForStandardInput =>
      webActor ! Notification(InputOutputApi.WaitingForStandardInput, Unused)

    case ContentRootManagerProtocol.ContentRootsAddedNotification(roots) =>
      roots.foreach { root =>
        webActor ! Notification(
          FileManagerApi.ContentRootAdded,
          FileManagerApi.ContentRootAdded.Params(root.toContentRoot)
        )
      }

    case RefactoringProtocol.ProjectRenamedNotification(
          oldNormalizedName,
          newNormalizedName,
          newName
        ) =>
      webActor ! Notification(
        RefactoringApi.ProjectRenamed,
        RefactoringApi.ProjectRenamed.Params(
          oldNormalizedName,
          newNormalizedName,
          newName
        )
      )

    case Api.ProgressNotification(payload) =>
      val translated: Notification[_, _] =
        translateProgressNotification(payload)
      webActor ! translated

    case req @ Request(method, _, _) if requestHandlers.contains(method) =>
      refreshIdleTime(method)
      val handler = context.actorOf(
        requestHandlers(method),
        s"request-handler-$method-${UUID.randomUUID()}"
      )
      handler.forward(req)
  }

  private def refreshIdleTime(method: Method): Unit = {
    method match {
      case InitialPing | Ping =>
      // ignore
      case _ =>
        idlenessMonitor ! MonitoringProtocol.ResetIdleTimeCommand
    }
  }

  private def createRequestHandlers(
    rpcSession: JsonSession
  ): Map[Method, Props] = {
    Map(
      Ping -> PingHandler.props(
        List(
          bufferRegistry,
          capabilityRouter,
          fileManager,
          contextRegistry
        ),
        requestTimeout
      ),
      InitialPing -> InitialPingHandler.props,
      AcquireCapability -> AcquireCapabilityHandler
        .props(capabilityRouter, requestTimeout, rpcSession),
      ReleaseCapability -> ReleaseCapabilityHandler
        .props(capabilityRouter, requestTimeout, rpcSession),
      OpenBuffer -> OpenBufferHandler
        .props(bufferRegistry, requestTimeout, rpcSession),
      OpenFile -> OpenFileHandler
        .props(bufferRegistry, requestTimeout, rpcSession),
      CloseFile -> CloseFileHandler
        .props(bufferRegistry, requestTimeout, rpcSession),
      ApplyEdit -> ApplyEditHandler
        .props(bufferRegistry, requestTimeout, rpcSession),
      ApplyExpressionValue -> ApplyExpressionValueHandler
        .props(bufferRegistry, requestTimeout, rpcSession),
      SaveFile -> SaveFileHandler
        .props(bufferRegistry, requestTimeout, rpcSession),
      WriteFile -> file.WriteTextualFileHandler
        .props(requestTimeout, fileManager),
      ReadFile -> file.ReadTextualFileHandler
        .props(requestTimeout, fileManager),
      CreateFile -> file.CreateFileHandler.props(requestTimeout, fileManager),
      DeleteFile -> file.DeleteFileHandler.props(requestTimeout, fileManager),
      CopyFile   -> file.CopyFileHandler.props(requestTimeout, fileManager),
      MoveFile   -> file.MoveFileHandler.props(requestTimeout, fileManager),
      ExistsFile -> file.ExistsFileHandler.props(requestTimeout, fileManager),
      ListFile   -> file.ListFileHandler.props(requestTimeout, fileManager),
      TreeFile   -> file.TreeFileHandler.props(requestTimeout, fileManager),
      InfoFile   -> file.InfoFileHandler.props(requestTimeout, fileManager),
      ChecksumFile -> file.ChecksumFileHandler
        .props(requestTimeout, fileManager),
      InitVcs -> vcs.InitVcsHandler
        .props(requestTimeout, bufferRegistry, rpcSession),
      SaveVcs -> vcs.SaveVcsHandler
        .props(requestTimeout, bufferRegistry, rpcSession),
      StatusVcs -> vcs.StatusVcsHandler
        .props(requestTimeout, vcsManager, rpcSession),
      RestoreVcs -> vcs.RestoreVcsHandler
        .props(requestTimeout, bufferRegistry, rpcSession),
      ListVcs -> vcs.ListVcsHandler
        .props(requestTimeout, vcsManager, rpcSession),
      ExecutionContextCreate -> executioncontext.CreateHandler
        .props(requestTimeout, contextRegistry, rpcSession),
      ExecutionContextDestroy -> executioncontext.DestroyHandler
        .props(requestTimeout, contextRegistry, rpcSession),
      ExecutionContextPush -> executioncontext.PushHandler
        .props(requestTimeout, contextRegistry, rpcSession),
      ExecutionContextPop -> executioncontext.PopHandler
        .props(requestTimeout, contextRegistry, rpcSession),
      ExecutionContextRecompute -> executioncontext.RecomputeHandler
        .props(requestTimeout, contextRegistry, rpcSession),
      ExecutionContextSetExecutionEnvironment -> executioncontext.SetExecutionEnvironmentHandler
        .props(requestTimeout, contextRegistry, rpcSession),
      ExecutionContextInterrupt -> executioncontext.InterruptHandler
        .props(requestTimeout, contextRegistry, rpcSession),
      ExecutionContextGetComponentGroups -> executioncontext.GetComponentGroupsHandler
        .props(requestTimeout, contextRegistry, rpcSession.clientId),
      GetSuggestionsDatabaseVersion -> search.GetSuggestionsDatabaseVersionHandler
        .props(requestTimeout, suggestionsHandler),
      GetSuggestionsDatabase -> search.GetSuggestionsDatabaseHandler
        .props(requestTimeout, suggestionsHandler),
      InvalidateSuggestionsDatabase -> search.InvalidateSuggestionsDatabaseHandler
        .props(requestTimeout, suggestionsHandler),
      AICompletion -> ai.AICompletionHandler.props(
        languageServerConfig.aiCompletionConfig
      ),
      ExecuteExpression -> ExecuteExpressionHandler
        .props(rpcSession.clientId, requestTimeout, contextRegistry),
      AttachVisualization -> AttachVisualizationHandler
        .props(rpcSession.clientId, requestTimeout, contextRegistry),
      DetachVisualization -> DetachVisualizationHandler
        .props(rpcSession.clientId, requestTimeout, contextRegistry),
      ModifyVisualization -> ModifyVisualizationHandler
        .props(rpcSession.clientId, requestTimeout, contextRegistry),
      RedirectStandardOutput -> RedirectStdOutHandler
        .props(stdOutController, rpcSession.clientId),
      SuppressStandardOutput -> SuppressStdOutHandler
        .props(stdOutController, rpcSession.clientId),
      SuppressStandardError -> SuppressStdErrHandler
        .props(stdErrController, rpcSession.clientId),
      RedirectStandardError -> RedirectStdErrHandler
        .props(stdErrController, rpcSession.clientId),
      FeedStandardInput -> FeedStandardInputHandler.props(stdInController),
      ProjectInfo       -> ProjectInfoHandler.props(languageServerConfig),
      EditionsGetProjectSettings -> EditionsGetProjectSettingsHandler
        .props(requestTimeout, projectSettingsManager),
      EditionsListAvailable -> EditionsListAvailableHandler.props(
        libraryConfig.editionManager
      ),
      EditionsListDefinedLibraries -> EditionsListDefinedLibrariesHandler
        .props(
          libraryConfig.editionReferenceResolver,
          libraryConfig.localLibraryProvider,
          libraryConfig.publishedLibraryCache
        ),
      EditionsResolve -> EditionsResolveHandler
        .props(libraryConfig.editionReferenceResolver),
      EditionsSetParentEdition -> EditionsSetParentEditionHandler
        .props(requestTimeout, projectSettingsManager),
      EditionsSetLocalLibrariesPreference -> EditionsSetProjectLocalLibrariesPreferenceHandler
        .props(requestTimeout, projectSettingsManager),
      EditionsListDefinedComponents -> EditionsListDefinedComponentsHandler
        .props(
          libraryConfig.editionReferenceResolver,
          libraryConfig.localLibraryProvider,
          libraryConfig.publishedLibraryCache
        ),
      LibraryCreate -> LibraryCreateHandler
        .props(requestTimeout, libraryConfig.localLibraryManager),
      LibraryListLocal -> LibraryListLocalHandler
        .props(requestTimeout, libraryConfig.localLibraryManager),
      LibraryGetMetadata -> LibraryGetMetadataHandler
        .props(
          requestTimeout,
          libraryConfig.localLibraryManager,
          libraryConfig.publishedLibraryCache
        ),
      LibraryPreinstall -> LibraryPreinstallHandler
        .props(libraryConfig.editionReferenceResolver, libraryConfig),
      LibraryPublish -> LibraryPublishHandler
        .props(requestTimeout, libraryConfig.localLibraryManager),
      LibrarySetMetadata -> LibrarySetMetadataHandler
        .props(requestTimeout, libraryConfig.localLibraryManager),
      LibraryGetPackage -> LibraryGetPackageHandler.props(
        requestTimeout,
        libraryConfig.localLibraryManager,
        libraryConfig.publishedLibraryCache
      ),
      RenameProject -> RenameProjectHandler.props(
        requestTimeout,
        runtimeConnector
      ),
      RenameSymbol -> RenameSymbolHandler.props(
        requestTimeout,
        runtimeConnector
      ),
      RuntimeGetComponentGroups -> runtime.GetComponentGroupsHandler.props(
        requestTimeout,
        runtimeConnector
      ),
      ProfilingStart -> ProfilingStartHandler
        .props(requestTimeout, profilingManager),
      ProfilingStop -> ProfilingStopHandler.props(
        requestTimeout,
        profilingManager
      ),
      ProfilingSnapshot -> ProfilingSnapshotHandler.props(
        requestTimeout,
        profilingManager
      )
    )
  }

  private def translateProgressNotification(
    progressNotification: ProgressNotification.NotificationType
  ): Notification[_, _] = progressNotification match {
    case ProgressNotification.TaskStarted(
          taskId,
          relatedOperation,
          unitStr,
          total
        ) =>
      val unit = ProgressUnit.fromString(unitStr)
      Notification(
        TaskNotificationApi.TaskStarted,
        TaskNotificationApi.TaskStarted
          .Params(taskId, relatedOperation, unit, total)
      )
    case ProgressNotification.TaskProgressUpdate(taskId, message, done) =>
      Notification(
        TaskNotificationApi.TaskProgressUpdate,
        TaskNotificationApi.TaskProgressUpdate.Params(taskId, message, done)
      )
    case ProgressNotification.TaskFinished(taskId, message, success) =>
      Notification(
        TaskNotificationApi.TaskFinished,
        TaskNotificationApi.TaskFinished.Params(taskId, message, success)
      )
  }
}

object JsonConnectionController {

  /** Creates a configuration object used to create a [[JsonConnectionController]].
    *
    * @param connectionId the internal connection id
    * @param mainComponent the main initialization logic
    * @param bufferRegistry a router that dispatches text editing requests
    * @param capabilityRouter a router that dispatches capability requests
    * @param fileManager performs operations with file system
    * @param vcsManager performs operations with VCS
    * @param contentRootManager manages the available content roots
    * @param contextRegistry a router that dispatches execution context requests
    * @param suggestionsHandler a reference to the suggestions requests handler
    * @param runtimeConnector a reference to the runtime connector
    * @param idlenessMonitor a reference to the idleness monitor actor
    * @param projectSettingsManager a reference to the project settings manager
    * @param profilingManager a reference to the profiling manager
    * @param libraryConfig configuration of the library ecosystem
    * @param requestTimeout a request timeout
    * @return a configuration object
    */
  def props(
    connectionId: UUID,
    mainComponent: InitializationComponent,
    bufferRegistry: ActorRef,
    capabilityRouter: ActorRef,
    fileManager: ActorRef,
    vcsManager: ActorRef,
    contentRootManager: ActorRef,
    contextRegistry: ActorRef,
    suggestionsHandler: ActorRef,
    stdOutController: ActorRef,
    stdErrController: ActorRef,
    stdInController: ActorRef,
    runtimeConnector: ActorRef,
    idlenessMonitor: ActorRef,
    projectSettingsManager: ActorRef,
    profilingManager: ActorRef,
    libraryConfig: LibraryConfig,
    languageServerConfig: Config,
    requestTimeout: FiniteDuration = 10.seconds
  ): Props =
    Props(
      new JsonConnectionController(
        connectionId           = connectionId,
        mainComponent          = mainComponent,
        bufferRegistry         = bufferRegistry,
        capabilityRouter       = capabilityRouter,
        fileManager            = fileManager,
        vcsManager             = vcsManager,
        contentRootManager     = contentRootManager,
        contextRegistry        = contextRegistry,
        suggestionsHandler     = suggestionsHandler,
        stdOutController       = stdOutController,
        stdErrController       = stdErrController,
        stdInController        = stdInController,
        runtimeConnector       = runtimeConnector,
        idlenessMonitor        = idlenessMonitor,
        projectSettingsManager = projectSettingsManager,
        profilingManager       = profilingManager,
        libraryConfig          = libraryConfig,
        languageServerConfig   = languageServerConfig,
        requestTimeout         = requestTimeout
      )
    )

}
