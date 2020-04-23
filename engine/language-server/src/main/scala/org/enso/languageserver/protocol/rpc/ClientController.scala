package org.enso.languageserver.protocol.rpc

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
import org.enso.languageserver.data.Client
import org.enso.languageserver.event.{ClientConnected, ClientDisconnected}
import org.enso.languageserver.filemanager.FileManagerApi._
import org.enso.languageserver.filemanager.PathWatcherProtocol
import org.enso.languageserver.monitoring.MonitoringApi.Ping
import org.enso.languageserver.requesthandler._
import org.enso.languageserver.requesthandler.capability._
import org.enso.languageserver.requesthandler.monitoring.PingHandler
import org.enso.languageserver.requesthandler.session.InitProtocolConnectionHandler
import org.enso.languageserver.requesthandler.text._
import org.enso.languageserver.runtime.ContextRegistryProtocol
import org.enso.languageserver.runtime.ExecutionApi._
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
  * @param server the language server actor ref.
  * @param bufferRegistry a router that dispatches text editing requests
  * @param capabilityRouter a router that dispatches capability requests
  * @param fileManager performs operations with file system
  * @param contextRegistry a router that dispatches execution context requests
  * @param requestTimeout a request timeout
  */
class ClientController(
  val connectionId: UUID,
  val server: ActorRef,
  val bufferRegistry: ActorRef,
  val capabilityRouter: ActorRef,
  val fileManager: ActorRef,
  val contextRegistry: ActorRef,
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
            server,
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
      val client = Client(clientId, self)
      context.system.eventStream.publish(ClientConnected(client))
      val requestHandlers = createRequestHandlers(client)
      val handler = context.actorOf(
        InitProtocolConnectionHandler.props(fileManager, requestTimeout)
      )
      handler.forward(req)
      context.become(initialised(webActor, client, requestHandlers))

    case Request(_, id, _) =>
      sender() ! ResponseError(Some(id), SessionNotInitialisedError)

    case MessageHandler.Disconnected =>
      context.stop(self)
  }

  private def initialised(
    webActor: ActorRef,
    client: Client,
    requestHandlers: Map[Method, Props]
  ): Receive = {
    case Request(InitProtocolConnection, id, _) =>
      sender() ! ResponseError(Some(id), SessionAlreadyInitialisedError)

    case MessageHandler.Disconnected =>
      context.system.eventStream.publish(ClientDisconnected(client))
      context.stop(self)

    case CapabilityProtocol.CapabilityForceReleased(registration) =>
      webActor ! Notification(ForceReleaseCapability, registration)

    case CapabilityProtocol.CapabilityGranted(registration) =>
      webActor ! Notification(GrantCapability, registration)

    case TextProtocol.TextDidChange(changes) =>
      webActor ! Notification(TextDidChange, TextDidChange.Params(changes))

    case PathWatcherProtocol.FileEventResult(event) =>
      webActor ! Notification(EventFile, EventFile.Params(event))

    case ContextRegistryProtocol
          .ExpressionValuesComputedNotification(contextId, updates) =>
      webActor ! Notification(
        ExecutionContextExpressionValuesComputed,
        ExecutionContextExpressionValuesComputed.Params(contextId, updates)
      )

    case req @ Request(method, _, _) if (requestHandlers.contains(method)) =>
      val handler = context.actorOf(requestHandlers(method))
      handler.forward(req)
  }

  private def createRequestHandlers(client: Client): Map[Method, Props] =
    Map(
      Ping -> PingHandler.props(
        List(
          server,
          bufferRegistry,
          capabilityRouter,
          fileManager,
          contextRegistry
        ),
        requestTimeout
      ),
      AcquireCapability -> AcquireCapabilityHandler
        .props(capabilityRouter, requestTimeout, client),
      ReleaseCapability -> ReleaseCapabilityHandler
        .props(capabilityRouter, requestTimeout, client),
      OpenFile -> OpenFileHandler.props(bufferRegistry, requestTimeout, client),
      CloseFile -> CloseFileHandler
        .props(bufferRegistry, requestTimeout, client),
      ApplyEdit -> ApplyEditHandler
        .props(bufferRegistry, requestTimeout, client),
      SaveFile   -> SaveFileHandler.props(bufferRegistry, requestTimeout, client),
      WriteFile  -> file.WriteFileHandler.props(requestTimeout, fileManager),
      ReadFile   -> file.ReadFileHandler.props(requestTimeout, fileManager),
      CreateFile -> file.CreateFileHandler.props(requestTimeout, fileManager),
      DeleteFile -> file.DeleteFileHandler.props(requestTimeout, fileManager),
      CopyFile   -> file.CopyFileHandler.props(requestTimeout, fileManager),
      MoveFile   -> file.MoveFileHandler.props(requestTimeout, fileManager),
      ExistsFile -> file.ExistsFileHandler.props(requestTimeout, fileManager),
      ListFile   -> file.ListFileHandler.props(requestTimeout, fileManager),
      TreeFile   -> file.TreeFileHandler.props(requestTimeout, fileManager),
      InfoFile   -> file.InfoFileHandler.props(requestTimeout, fileManager),
      ExecutionContextCreate -> executioncontext.CreateHandler
        .props(requestTimeout, contextRegistry, client),
      ExecutionContextDestroy -> executioncontext.DestroyHandler
        .props(requestTimeout, contextRegistry, client),
      ExecutionContextPush -> executioncontext.PushHandler
        .props(requestTimeout, contextRegistry, client),
      ExecutionContextPop -> executioncontext.PopHandler
        .props(requestTimeout, contextRegistry, client)
    )

}

object ClientController {

  /**
    * Creates a configuration object used to create a [[ClientController]].
    *
    * @param clientId the internal client id.
    * @param server the language server actor ref.
    * @param bufferRegistry a router that dispatches text editing requests
    * @param capabilityRouter a router that dispatches capability requests
    * @param fileManager performs operations with file system
    * @param contextRegistry a router that dispatches execution context requests
    * @param requestTimeout a request timeout
    * @return a configuration object
    */
  def props(
    clientId: UUID,
    server: ActorRef,
    bufferRegistry: ActorRef,
    capabilityRouter: ActorRef,
    fileManager: ActorRef,
    contextRegistry: ActorRef,
    requestTimeout: FiniteDuration = 10.seconds
  ): Props =
    Props(
      new ClientController(
        clientId,
        server,
        bufferRegistry,
        capabilityRouter,
        fileManager,
        contextRegistry,
        requestTimeout
      )
    )

}
