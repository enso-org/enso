package org.enso.projectmanager.infrastructure.languageserver

import java.util.UUID

import akka.actor.{Actor, ActorRef, Cancellable, Props, Scheduler, Terminated}
import com.typesafe.scalalogging.LazyLogging
import org.enso.projectmanager.boot.configuration.SupervisionConfig
import org.enso.projectmanager.data.Socket
import org.enso.projectmanager.infrastructure.http.WebSocketConnectionFactory
import org.enso.projectmanager.infrastructure.languageserver.LanguageServerBootLoader.{
  ServerBootFailed,
  ServerBooted
}
import org.enso.projectmanager.infrastructure.languageserver.LanguageServerController.ServerDied
import org.enso.projectmanager.infrastructure.languageserver.LanguageServerSupervisor.{
  SendHeartbeat,
  ServerUnresponsive,
  StartSupervision
}
import org.enso.projectmanager.util.UnhandledLogging

/** A supervisor process responsible for monitoring language server and
  * restarting it when the server is unresponsive. It delegates server
  * monitoring to the [[HeartbeatSession]] actor.
  *
  * @param connectionInfo a server connection info
  * @param serverProcessManager an actor that manages the lifecycle of the
  *                             server process
  * @param supervisionConfig a supervision config
  * @param connectionFactory a web socket connection factory
  * @param scheduler a scheduler
  */
class LanguageServerSupervisor(
  connectionInfo: LanguageServerConnectionInfo,
  serverProcessManager: ActorRef,
  supervisionConfig: SupervisionConfig,
  connectionFactory: WebSocketConnectionFactory,
  scheduler: Scheduler
) extends Actor
    with LazyLogging
    with UnhandledLogging {

  import context.dispatcher

  override def preStart(): Unit = { self ! StartSupervision }

  override def receive: Receive = uninitialized

  private def uninitialized: Receive = {
    case GracefulStop =>
      context.stop(self)

    case StartSupervision =>
      val cancellable =
        scheduler.scheduleAtFixedRate(
          supervisionConfig.initialDelay,
          supervisionConfig.heartbeatInterval,
          self,
          SendHeartbeat
        )
      context.become(supervising(cancellable))
  }

  private def supervising(cancellable: Cancellable): Receive = {
    case SendHeartbeat =>
      val socket = Socket(connectionInfo.interface, connectionInfo.rpcPort)
      context.actorOf(
        HeartbeatSession.props(
          socket,
          supervisionConfig.heartbeatTimeout,
          connectionFactory,
          scheduler
        ),
        s"heartbeat-${UUID.randomUUID()}"
      )

    case ServerUnresponsive =>
      cancellable.cancel()
      logger.info("Server is unresponsive. Restarting [{}].", connectionInfo)
      serverProcessManager ! Restart
      context.become(restarting)

    case GracefulStop =>
      cancellable.cancel()
      stop()
  }

  private def restarting: Receive = {
    case ServerBootFailed(_) =>
      logger.error("Cannot restart language server.")
      context.parent ! ServerDied
      context.stop(self)

    case ServerBooted(_, newProcessManager) =>
      if (newProcessManager != serverProcessManager) {
        logger.error(
          "The process manager actor has changed. This should never happen. " +
          "Supervisor may no longer work correctly."
        )
      }
      logger.info("Language server restarted [{}].", connectionInfo)
      val cancellable =
        scheduler.scheduleAtFixedRate(
          supervisionConfig.initialDelay,
          supervisionConfig.heartbeatInterval,
          self,
          SendHeartbeat
        )
      context.become(supervising(cancellable))

    case GracefulStop =>
      stop()
  }

  private def waitingForChildren(): Receive = { case Terminated(_) =>
    if (context.children.isEmpty) {
      context.stop(self)
    }
  }

  private def stop(): Unit = {
    if (context.children.isEmpty) {
      context.stop(self)
    } else {
      context.children.foreach(_ ! GracefulStop)
      context.children.foreach(context.watch)
      context.become(waitingForChildren())
    }
  }

}

object LanguageServerSupervisor {

  private case object StartSupervision

  /** A command responsible for initiating heartbeat session.
    */
  case object SendHeartbeat

  /** Signals that server is unresponsive.
    */
  case object ServerUnresponsive

  /** Signals that the heartbeat has been received (only sent if demanded). */
  case object HeartbeatReceived

  /** Creates a configuration object used to create a [[LanguageServerSupervisor]].
    *
    * @param connectionInfo a server config
    * @param serverProcessManager an actor that manages the lifecycle of the
    *                             server process
    * @param supervisionConfig a supervision config
    * @param connectionFactory a web socket connection factory
    * @param scheduler a scheduler
    * @return a configuration object
    */
  def props(
    connectionInfo: LanguageServerConnectionInfo,
    serverProcessManager: ActorRef,
    supervisionConfig: SupervisionConfig,
    connectionFactory: WebSocketConnectionFactory,
    scheduler: Scheduler
  ): Props =
    Props(
      new LanguageServerSupervisor(
        connectionInfo,
        serverProcessManager,
        supervisionConfig,
        connectionFactory,
        scheduler
      )
    )

}
