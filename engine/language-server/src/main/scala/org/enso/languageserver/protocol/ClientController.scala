package org.enso.languageserver.protocol

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
import org.enso.languageserver.filemanager.PathWatcherProtocol
import org.enso.languageserver.filemanager.FileManagerApi._
import org.enso.languageserver.requesthandler._
import org.enso.languageserver.text.TextApi._
import org.enso.languageserver.text.TextProtocol

import scala.concurrent.duration._

/**
  * An actor handling communications between a single client and the language
  * server.
  *
  * @param clientId the internal client id.
  * @param server the language server actor ref.
  * @param bufferRegistry a router that dispatches text editing requests
  * @param capabilityRouter a router that dispatches capability requests
  * @param requestTimeout a request timeout
  */
class ClientController(
  val clientId: Client.Id,
  val server: ActorRef,
  val bufferRegistry: ActorRef,
  val capabilityRouter: ActorRef,
  val fileManager: ActorRef,
  requestTimeout: FiniteDuration = 10.seconds
) extends Actor
    with Stash
    with ActorLogging {

  implicit val timeout = Timeout(requestTimeout)

  private val client = Client(clientId, self)

  private val requestHandlers: Map[Method, Props] =
    Map(
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
      InfoFile   -> file.InfoFileHandler.props(requestTimeout, fileManager)
    )

  override def unhandled(message: Any): Unit =
    log.warning("Received unknown message: {}", message)

  override def receive: Receive = {
    case JsonRpcServer.WebConnect(webActor) =>
      context.system.eventStream
        .publish(ClientConnected(Client(clientId, self)))
      unstashAll()
      context.become(connected(webActor))
    case _ => stash()
  }

  def connected(webActor: ActorRef): Receive = {
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

    case r @ Request(method, _, _) if (requestHandlers.contains(method)) =>
      val handler = context.actorOf(requestHandlers(method))
      handler.forward(r)
  }
}

object ClientController {

  /**
    * Creates a configuration object used to create a [[ClientController]].
    *
    * @param clientId the internal client id.
    * @param server the language server actor ref.
    * @param bufferRegistry a router that dispatches text editing requests
    * @param capabilityRouter a router that dispatches capability requests
    * @param requestTimeout a request timeout
    * @return a configuration object
    */
  def props(
    clientId: UUID,
    server: ActorRef,
    bufferRegistry: ActorRef,
    capabilityRouter: ActorRef,
    fileManager: ActorRef,
    requestTimeout: FiniteDuration = 10.seconds
  ): Props =
    Props(
      new ClientController(
        clientId,
        server,
        bufferRegistry,
        capabilityRouter,
        fileManager,
        requestTimeout
      )
    )

}
