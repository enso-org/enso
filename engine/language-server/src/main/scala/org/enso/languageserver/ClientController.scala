package org.enso.languageserver

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Stash}
import akka.pattern.ask
import akka.util.Timeout
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
import org.enso.languageserver.filemanager.FileManagerProtocol.{
  CreateFileResult,
  WriteFileResult
}
import org.enso.languageserver.filemanager.{
  FileManagerProtocol,
  FileSystemFailureMapper
}
import org.enso.languageserver.jsonrpc.Errors.ServiceError
import org.enso.languageserver.jsonrpc._
import org.enso.languageserver.requesthandler.{
  AcquireCapabilityHandler,
  ApplyEditHandler,
  CloseFileHandler,
  OpenFileHandler,
  ReleaseCapabilityHandler,
  SaveFileHandler
}
import org.enso.languageserver.text.TextApi.{
  ApplyEdit,
  CloseFile,
  OpenFile,
  SaveFile,
  TextDidChange
}
import org.enso.languageserver.text.TextProtocol

import scala.concurrent.duration._
import scala.util.{Failure, Success}

/**
  * The JSON RPC API provided by the language server.
  * See [[https://github.com/luna/enso/blob/master/doc/design/engine/engine-services.md]]
  * for message specifications.
  */
object ClientApi {
  import io.circe.generic.auto._

  val protocol: Protocol = Protocol.empty
    .registerRequest(AcquireCapability)
    .registerRequest(ReleaseCapability)
    .registerRequest(WriteFile)
    .registerRequest(ReadFile)
    .registerRequest(CreateFile)
    .registerRequest(OpenFile)
    .registerRequest(CloseFile)
    .registerRequest(SaveFile)
    .registerRequest(ApplyEdit)
    .registerRequest(DeleteFile)
    .registerRequest(CopyFile)
    .registerRequest(MoveFile)
    .registerRequest(ExistsFile)
    .registerRequest(TreeFile)
    .registerNotification(ForceReleaseCapability)
    .registerNotification(GrantCapability)
    .registerNotification(TextDidChange)

  case class WebConnect(webActor: ActorRef)
}

/**
  * An actor handling communications between a single client and the language
  * server.
  *
  * @param clientId the internal client id.
  * @param server the language server actor.
  */
class ClientController(
  val clientId: Client.Id,
  val server: ActorRef,
  val bufferRegistry: ActorRef,
  val capabilityRouter: ActorRef,
  requestTimeout: FiniteDuration = 10.seconds
) extends Actor
    with Stash
    with ActorLogging {

  import context.dispatcher

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
      SaveFile -> SaveFileHandler.props(bufferRegistry, requestTimeout, client)
    )

  override def unhandled(message: Any): Unit =
    log.warning("Received unknown message: {}", message)

  override def receive: Receive = {
    case ClientApi.WebConnect(webActor) =>
      context.system.eventStream
        .publish(ClientConnected(Client(clientId, self)))
      unstashAll()
      context.become(connected(webActor))
    case _ => stash()
  }

  def connected(webActor: ActorRef): Receive = {
    case MessageHandler.Disconnected =>
      context.system.eventStream.publish(ClientDisconnected(clientId))
      context.stop(self)

    case CapabilityProtocol.CapabilityForceReleased(registration) =>
      webActor ! Notification(ForceReleaseCapability, registration)

    case CapabilityProtocol.CapabilityGranted(registration) =>
      webActor ! Notification(GrantCapability, registration)

    case TextProtocol.TextDidChange(changes) =>
      webActor ! Notification(TextDidChange, TextDidChange.Params(changes))

    case r @ Request(method, _, _) if (requestHandlers.contains(method)) =>
      val handler = context.actorOf(requestHandlers(method))
      handler.forward(r)

    case Request(WriteFile, id, params: WriteFile.Params) =>
      writeFile(webActor, id, params)

    case Request(ReadFile, id, params: ReadFile.Params) =>
      readFile(webActor, id, params)

    case Request(CreateFile, id, params: CreateFile.Params) =>
      createFile(webActor, id, params)

    case Request(DeleteFile, id, params: DeleteFile.Params) =>
      deleteFile(webActor, id, params)

    case Request(CopyFile, id, params: CopyFile.Params) =>
      copyFile(webActor, id, params)

    case Request(MoveFile, id, params: MoveFile.Params) =>
      moveFile(webActor, id, params)

    case Request(ExistsFile, id, params: ExistsFile.Params) =>
      existsFile(webActor, id, params)

    case Request(TreeFile, id, params: TreeFile.Params) =>
      treeFile(webActor, id, params)
  }

  private def readFile(
    webActor: ActorRef,
    id: Id,
    params: ReadFile.Params
  ): Unit = {
    (server ? FileManagerProtocol.ReadFile(params.path)).onComplete {
      case Success(
          FileManagerProtocol.ReadFileResult(Right(content: String))
          ) =>
        webActor ! ResponseResult(ReadFile, id, ReadFile.Result(content))

      case Success(FileManagerProtocol.ReadFileResult(Left(failure))) =>
        webActor ! ResponseError(
          Some(id),
          FileSystemFailureMapper.mapFailure(failure)
        )

      case Failure(th) =>
        log.error("An exception occurred during reading a file", th)
        webActor ! ResponseError(Some(id), ServiceError)
    }
  }

  private def writeFile(
    webActor: ActorRef,
    id: Id,
    params: WriteFile.Params
  ): Unit = {
    (server ? FileManagerProtocol.WriteFile(params.path, params.contents))
      .onComplete {
        case Success(WriteFileResult(Right(()))) =>
          webActor ! ResponseResult(WriteFile, id, Unused)

        case Success(WriteFileResult(Left(failure))) =>
          webActor ! ResponseError(
            Some(id),
            FileSystemFailureMapper.mapFailure(failure)
          )

        case Failure(th) =>
          log.error("An exception occurred during writing to a file", th)
          webActor ! ResponseError(Some(id), ServiceError)
      }
  }

  private def createFile(
    webActor: ActorRef,
    id: Id,
    params: CreateFile.Params
  ): Unit = {
    (server ? FileManagerProtocol.CreateFile(params.`object`))
      .onComplete {
        case Success(CreateFileResult(Right(()))) =>
          webActor ! ResponseResult(CreateFile, id, Unused)

        case Success(CreateFileResult(Left(failure))) =>
          webActor ! ResponseError(
            Some(id),
            FileSystemFailureMapper.mapFailure(failure)
          )

        case Failure(th) =>
          log.error("An exception occurred during creating a file", th)
          webActor ! ResponseError(Some(id), ServiceError)
      }
  }

  private def deleteFile(
    webActor: ActorRef,
    id: Id,
    params: DeleteFile.Params
  ): Unit = {
    (server ? FileManagerProtocol.DeleteFile(params.path))
      .onComplete {
        case Success(FileManagerProtocol.DeleteFileResult(Right(()))) =>
          webActor ! ResponseResult(DeleteFile, id, Unused)

        case Success(FileManagerProtocol.DeleteFileResult(Left(failure))) =>
          webActor ! ResponseError(
            Some(id),
            FileSystemFailureMapper.mapFailure(failure)
          )

        case Failure(th) =>
          log.error("An exception occurred during deleting a file", th)
          webActor ! ResponseError(Some(id), ServiceError)
      }
  }

  private def copyFile(
    webActor: ActorRef,
    id: Id,
    params: CopyFile.Params
  ): Unit = {
    (server ? FileManagerProtocol.CopyFile(params.from, params.to))
      .onComplete {
        case Success(FileManagerProtocol.CopyFileResult(Right(()))) =>
          webActor ! ResponseResult(CopyFile, id, Unused)

        case Success(FileManagerProtocol.CopyFileResult(Left(failure))) =>
          webActor ! ResponseError(
            Some(id),
            FileSystemFailureMapper.mapFailure(failure)
          )

        case Failure(th) =>
          log.error("An exception occured during copying a file", th)
          webActor ! ResponseError(Some(id), ServiceError)
      }
  }

  private def moveFile(
    webActor: ActorRef,
    id: Id,
    params: MoveFile.Params
  ): Unit = {
    (server ? FileManagerProtocol.MoveFile(params.from, params.to))
      .onComplete {
        case Success(FileManagerProtocol.MoveFileResult(Right(()))) =>
          webActor ! ResponseResult(MoveFile, id, Unused)

        case Success(FileManagerProtocol.MoveFileResult(Left(failure))) =>
          webActor ! ResponseError(
            Some(id),
            FileSystemFailureMapper.mapFailure(failure)
          )

        case Failure(th) =>
          log.error("An exception occured during moving a file", th)
          webActor ! ResponseError(Some(id), ServiceError)
      }
  }

  private def existsFile(
    webActor: ActorRef,
    id: Id,
    params: ExistsFile.Params
  ): Unit = {
    (server ? FileManagerProtocol.ExistsFile(params.path))
      .onComplete {
        case Success(FileManagerProtocol.ExistsFileResult(Right(exists))) =>
          webActor ! ResponseResult(ExistsFile, id, ExistsFile.Result(exists))

        case Success(FileManagerProtocol.ExistsFileResult(Left(failure))) =>
          webActor ! ResponseError(
            Some(id),
            FileSystemFailureMapper.mapFailure(failure)
          )

        case Failure(th) =>
          log.error("An exception occurred during exists file command", th)
          webActor ! ResponseError(Some(id), ServiceError)
      }
  }

  private def treeFile(
    webActor: ActorRef,
    id: Id,
    params: TreeFile.Params
  ): Unit = {
    (server ? FileManagerProtocol.TreeFile(params.path, params.depth))
      .onComplete {
        case Success(FileManagerProtocol.TreeFileResult(Right(tree))) =>
          webActor ! ResponseResult(TreeFile, id, TreeFile.Result(tree))

        case Success(FileManagerProtocol.TreeFileResult(Left(failure))) =>
          webActor ! ResponseError(
            Some(id),
            FileSystemFailureMapper.mapFailure(failure)
          )

        case Failure(th) =>
          log.error("An exception occured during a tree operation", th)
          webActor ! ResponseError(Some(id), ServiceError)
      }
  }

}
