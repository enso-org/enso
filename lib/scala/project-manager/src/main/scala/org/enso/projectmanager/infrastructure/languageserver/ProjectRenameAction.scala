package org.enso.projectmanager.infrastructure.languageserver

import java.util.UUID

import akka.actor.{Actor, ActorLogging, ActorRef, Cancellable, Props, Scheduler}
import akka.stream.SubscriptionWithCancelException.StageWasCompleted
import io.circe
import io.circe.Json
import io.circe.parser.parse
import org.enso.projectmanager.data.Socket
import org.enso.projectmanager.infrastructure.http.AkkaBasedWebSocketConnectionFactory
import org.enso.projectmanager.infrastructure.http.WebSocketConnection.{
  WebSocketConnected,
  WebSocketMessage,
  WebSocketStreamClosed,
  WebSocketStreamFailure
}
import org.enso.projectmanager.infrastructure.languageserver.LanguageServerProtocol.{
  CannotConnectToServer,
  ProjectRenamed,
  RenameFailure,
  RenameTimeout,
  ServerUnresponsive
}
import org.enso.projectmanager.infrastructure.languageserver.ProjectRenameAction.{
  ActionTimeout,
  SocketClosureTimeout
}
import org.enso.projectmanager.util.UnhandledLogging

import scala.concurrent.duration.FiniteDuration

/** An action that encapsulates a request-reply flow responsible for refactoring
  * project name in a running language server.
  *
  * @param replyTo a recipient of a reply
  * @param socket a server socket
  * @param actionTimeout a time to complete whole action
  * @param socketCloseTimeout a socket close timeout
  * @param oldName an old project name
  * @param newName a new project name
  * @param scheduler a scheduler used to keep timeouts
  */
class ProjectRenameAction(
  replyTo: ActorRef,
  socket: Socket,
  actionTimeout: FiniteDuration,
  socketCloseTimeout: FiniteDuration,
  oldName: String,
  newName: String,
  scheduler: Scheduler
) extends Actor
    with ActorLogging
    with UnhandledLogging {

  import context.{dispatcher, system}

  private val requestId = UUID.randomUUID()

  private val connectionFactory = new AkkaBasedWebSocketConnectionFactory

  private val connection = connectionFactory.createConnection(socket)

  private var maybeActionTimeoutCancellable: Option[Cancellable] = None

  override def preStart(): Unit = {
    log.info(s"Requesting a Language Server to rename project $oldName")
    connection.attachListener(self)
    connection.connect()
    val cancellable =
      scheduler.scheduleOnce(actionTimeout, self, ActionTimeout)
    maybeActionTimeoutCancellable = Some(cancellable)
  }

  override def receive: Receive = unconnected()

  private def unconnected(): Receive = {
    case WebSocketConnected =>
      connection.send(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "method": "refactoring/renameProject",
           |   "id": "$requestId",
           |   "params": {
           |     "oldName": "$oldName",
           |     "newName": "$newName"
           |   }
           |}
           |""".stripMargin
      )
      context.become(connected())

    case WebSocketStreamFailure(th) =>
      log.error(th, s"An error occurred during connecting to websocket $socket")
      replyTo ! CannotConnectToServer
      stop()

    case ActionTimeout =>
      log.error("Action timeout occurred. Stopping actor.")
      replyTo ! RenameTimeout
      stop()

    case GracefulStop =>
      log.warning("Ignoring stop command")
  }

  private def connected(): Receive = {
    case WebSocketMessage(payload) =>
      val maybeError =
        parse(payload).flatMap(_.hcursor.downField("error").as[Json])

      if (maybeError.isRight) {
        handleError(maybeError)
      } else {
        handleSuccess(payload)
      }

    case WebSocketStreamClosed | WebSocketStreamFailure(StageWasCompleted) =>
      replyTo ! ServerUnresponsive
      context.stop(self)
      maybeActionTimeoutCancellable.foreach(_.cancel())

    case WebSocketStreamFailure(th) =>
      log.error(th, s"An error occurred during waiting for Pong message")
      replyTo ! ServerUnresponsive
      stop()

    case ActionTimeout =>
      log.error("Action timeout occurred. Stopping actor.")
      replyTo ! RenameTimeout
      stop()

    case GracefulStop =>
      log.warning("Ignoring stop command")
  }

  private def handleSuccess(payload: String): Unit = {
    val maybeRequestId =
      parse(payload).flatMap(_.hcursor.downField("id").as[String])

    maybeRequestId match {
      case Left(error) =>
        log.error(error, "An error occurred during parsing rename reply")

      case Right(id) =>
        if (id == requestId.toString) {
          log.info(s"Project renamed by the Language Server")
          replyTo ! ProjectRenamed
          stop()
        } else {
          log.warning(s"Received unknown response $payload")
        }
    }
  }

  private def handleError(maybeError: Either[circe.Error, Json]): Unit = {
    val code =
      maybeError.flatMap(_.hcursor.downField("code").as[Int]).getOrElse(0)
    val msg = maybeError
      .flatMap(_.hcursor.downField("message").as[String])
      .getOrElse("Not Provided")
    log.error(
      s"Error occurred during renaming project [code: $code message: $msg]"
    )
    replyTo ! RenameFailure(code, msg)
    stop()
  }

  private def socketClosureStage(
    closureTimeoutCancellable: Cancellable
  ): Receive = {
    case WebSocketStreamClosed | WebSocketStreamFailure(StageWasCompleted) =>
      context.stop(self)
      closureTimeoutCancellable.cancel()

    case WebSocketStreamFailure(th) =>
      log.error(th, s"An error occurred during closing web socket")
      context.stop(self)
      closureTimeoutCancellable.cancel()

    case SocketClosureTimeout =>
      log.error(s"Socket closure timed out")
      context.stop(self)

    case GracefulStop =>
      log.warning("Ignoring stop command")
  }

  private def stop(): Unit = {
    connection.disconnect()
    val closureTimeout =
      scheduler.scheduleOnce(socketCloseTimeout, self, SocketClosureTimeout)
    maybeActionTimeoutCancellable.foreach(_.cancel())
    context.become(socketClosureStage(closureTimeout))
  }

}

object ProjectRenameAction {

  /** Signals an action timeout.
    */
  case object ActionTimeout

  /** Signals a socket closure timeout.
    */
  case object SocketClosureTimeout

  /** Creates a configuration object used to create a [[ProjectRenameAction]].
    *
    * @param replyTo a recipient of a reply
    * @param socket a server socket
    * @param actionTimeout a time to complete whole action
    * @param socketCloseTimeout a socket close timeout
    * @param oldName an old project name
    * @param newName a new project name
    * @param scheduler a scheduler used to keep timeouts
    * @return a configuration object
    */
  def props(
    replyTo: ActorRef,
    socket: Socket,
    actionTimeout: FiniteDuration,
    socketCloseTimeout: FiniteDuration,
    oldName: String,
    newName: String,
    scheduler: Scheduler
  ): Props =
    Props(
      new ProjectRenameAction(
        replyTo,
        socket,
        actionTimeout,
        socketCloseTimeout,
        oldName,
        newName,
        scheduler
      )
    )

}
