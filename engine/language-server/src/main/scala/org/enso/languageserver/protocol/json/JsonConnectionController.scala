package org.enso.languageserver.protocol.json

import java.util.UUID

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Stash}
import akka.util.Timeout
import org.enso.jsonrpc._
import org.enso.languageserver.capability.CapabilityApi.{
  AcquireCapability,
  ForceReleaseCapability,
  GrantCapability,
  ReleaseCapability
}
import org.enso.languageserver.capability.CapabilityProtocol
import org.enso.languageserver.event.{
  JsonSessionInitialized,
  JsonSessionTerminated
}
import org.enso.languageserver.filemanager.FileManagerApi._
import org.enso.languageserver.filemanager.PathWatcherProtocol
import org.enso.languageserver.io.InputOutputApi._
import org.enso.languageserver.io.{InputOutputApi, InputOutputProtocol}
import org.enso.languageserver.io.OutputKind.{StandardError, StandardOutput}
import org.enso.languageserver.monitoring.MonitoringApi.Ping
import org.enso.languageserver.requesthandler._
import org.enso.languageserver.requesthandler.capability._
import org.enso.languageserver.requesthandler.io.{
  FeedStandardInputHandler,
  RedirectStdErrHandler,
  RedirectStdOutHandler,
  SuppressStdErrHandler,
  SuppressStdOutHandler
}
import org.enso.languageserver.requesthandler.monitoring.PingHandler
import org.enso.languageserver.requesthandler.session.InitProtocolConnectionHandler
import org.enso.languageserver.requesthandler.text._
import org.enso.languageserver.requesthandler.visualisation.{
  AttachVisualisationHandler,
  DetachVisualisationHandler,
  ModifyVisualisationHandler
}
import org.enso.languageserver.runtime.{
  ContextRegistryProtocol,
  SearchApi,
  SearchProtocol
}
import org.enso.languageserver.runtime.ExecutionApi._
import org.enso.languageserver.runtime.VisualisationApi.{
  AttachVisualisation,
  DetachVisualisation,
  ModifyVisualisation
}
import org.enso.languageserver.session.JsonSession
import org.enso.languageserver.session.SessionApi.{
  InitProtocolConnection,
  SessionAlreadyInitialisedError,
  SessionNotInitialisedError
}
import org.enso.languageserver.text.TextApi._
import org.enso.languageserver.text.TextProtocol
import org.enso.languageserver.util.UnhandledLogging

import scala.concurrent.duration._

/**
  * An actor handling communications between a single client and the language
  * server.
  *
  * @param connectionId the internal connection id.
  * @param bufferRegistry a router that dispatches text editing requests
  * @param capabilityRouter a router that dispatches capability requests
  * @param fileManager performs operations with file system
  * @param contextRegistry a router that dispatches execution context requests
  * @param requestTimeout a request timeout
  */
class JsonConnectionController(
  val connectionId: UUID,
  val bufferRegistry: ActorRef,
  val capabilityRouter: ActorRef,
  val fileManager: ActorRef,
  val contextRegistry: ActorRef,
  val stdOutController: ActorRef,
  val stdErrController: ActorRef,
  val stdInController: ActorRef,
  requestTimeout: FiniteDuration = 10.seconds
) extends Actor
    with Stash
    with ActorLogging
    with UnhandledLogging {

  implicit val timeout = Timeout(requestTimeout)

  override def receive: Receive = {
    case JsonRpcServer.WebConnect(webActor) =>
      unstashAll()
      context.become(connected(webActor))
    case _ => stash()
  }

  private def connected(webActor: ActorRef): Receive = {
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

    case req @ Request(
          InitProtocolConnection,
          _,
          InitProtocolConnection.Params(clientId)
        ) =>
      log.info(s"RPC session initialized for client: $clientId")
      val session = JsonSession(clientId, self)
      context.system.eventStream.publish(JsonSessionInitialized(session))
      val requestHandlers = createRequestHandlers(session)
      val handler = context.actorOf(
        InitProtocolConnectionHandler.props(fileManager, requestTimeout)
      )
      handler.forward(req)
      context.become(initialised(webActor, session, requestHandlers))

    case Request(_, id, _) =>
      sender() ! ResponseError(Some(id), SessionNotInitialisedError)

    case MessageHandler.Disconnected =>
      context.stop(self)
  }

  private def initialised(
    webActor: ActorRef,
    rpcSession: JsonSession,
    requestHandlers: Map[Method, Props]
  ): Receive = {
    case Request(InitProtocolConnection, id, _) =>
      sender() ! ResponseError(Some(id), SessionAlreadyInitialisedError)

    case MessageHandler.Disconnected =>
      context.system.eventStream.publish(JsonSessionTerminated(rpcSession))
      context.stop(self)

    case CapabilityProtocol.CapabilityForceReleased(registration) =>
      webActor ! Notification(ForceReleaseCapability, registration)

    case CapabilityProtocol.CapabilityGranted(registration) =>
      webActor ! Notification(GrantCapability, registration)

    case TextProtocol.TextDidChange(changes) =>
      webActor ! Notification(TextDidChange, TextDidChange.Params(changes))

    case PathWatcherProtocol.FileEventResult(event) =>
      webActor ! Notification(
        EventFile,
        EventFile.Params(event.path, event.kind)
      )

    case ContextRegistryProtocol
          .ExpressionValuesComputedNotification(contextId, updates) =>
      webActor ! Notification(
        ExecutionContextExpressionValuesComputed,
        ExecutionContextExpressionValuesComputed.Params(contextId, updates)
      )

    case ContextRegistryProtocol.ExecutionFailedNotification(contextId, msg) =>
      webActor ! Notification(
        ExecutionContextExecutionFailed,
        ExecutionContextExecutionFailed.Params(contextId, msg)
      )

    case SearchProtocol.SuggestionsDatabaseUpdateNotification(
          updates,
          version
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

    case req @ Request(method, _, _) if requestHandlers.contains(method) =>
      val handler = context.actorOf(
        requestHandlers(method),
        s"request-handler-$method-${UUID.randomUUID()}"
      )
      handler.forward(req)
  }

  private def createRequestHandlers(
    rpcSession: JsonSession
  ): Map[Method, Props] =
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
      AcquireCapability -> AcquireCapabilityHandler
        .props(capabilityRouter, requestTimeout, rpcSession),
      ReleaseCapability -> ReleaseCapabilityHandler
        .props(capabilityRouter, requestTimeout, rpcSession),
      OpenFile -> OpenFileHandler
        .props(bufferRegistry, requestTimeout, rpcSession),
      CloseFile -> CloseFileHandler
        .props(bufferRegistry, requestTimeout, rpcSession),
      ApplyEdit -> ApplyEditHandler
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
      AttachVisualisation -> AttachVisualisationHandler
        .props(rpcSession.clientId, requestTimeout, contextRegistry),
      DetachVisualisation -> DetachVisualisationHandler
        .props(rpcSession.clientId, requestTimeout, contextRegistry),
      ModifyVisualisation -> ModifyVisualisationHandler
        .props(rpcSession.clientId, requestTimeout, contextRegistry),
      RedirectStandardOutput -> RedirectStdOutHandler
        .props(stdOutController, rpcSession.clientId),
      SuppressStandardOutput -> SuppressStdOutHandler
        .props(stdOutController, rpcSession.clientId),
      SuppressStandardError -> SuppressStdErrHandler
        .props(stdErrController, rpcSession.clientId),
      RedirectStandardError -> RedirectStdErrHandler
        .props(stdErrController, rpcSession.clientId),
      FeedStandardInput -> FeedStandardInputHandler.props(stdInController)
    )

}

object JsonConnectionController {

  /**
    * Creates a configuration object used to create a [[JsonConnectionController]].
    *
    * @param connectionId the internal connection id.
    * @param bufferRegistry a router that dispatches text editing requests
    * @param capabilityRouter a router that dispatches capability requests
    * @param fileManager performs operations with file system
    * @param contextRegistry a router that dispatches execution context requests
    * @param requestTimeout a request timeout
    * @return a configuration object
    */
  def props(
    connectionId: UUID,
    bufferRegistry: ActorRef,
    capabilityRouter: ActorRef,
    fileManager: ActorRef,
    contextRegistry: ActorRef,
    stdOutController: ActorRef,
    stdErrController: ActorRef,
    stdInController: ActorRef,
    requestTimeout: FiniteDuration = 10.seconds
  ): Props =
    Props(
      new JsonConnectionController(
        connectionId,
        bufferRegistry,
        capabilityRouter,
        fileManager,
        contextRegistry,
        stdOutController,
        stdErrController,
        stdInController,
        requestTimeout
      )
    )

}
