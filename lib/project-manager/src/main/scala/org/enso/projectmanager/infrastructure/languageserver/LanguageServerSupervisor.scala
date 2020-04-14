package org.enso.projectmanager.infrastructure.languageserver

import akka.actor.Status.Failure
import akka.actor.{
  Actor,
  ActorLogging,
  Cancellable,
  Props,
  Scheduler,
  Terminated
}
import akka.pattern.pipe
import org.enso.languageserver.boot.LifecycleComponent.ComponentRestarted
import org.enso.languageserver.boot.{LanguageServerConfig, LifecycleComponent}
import org.enso.projectmanager.boot.configuration.SupervisionConfig
import org.enso.projectmanager.data.Socket
import org.enso.projectmanager.infrastructure.http.WebSocketConnectionFactory
import org.enso.projectmanager.infrastructure.languageserver.LanguageServerController.ServerDied
import org.enso.projectmanager.infrastructure.languageserver.LanguageServerSupervisor.{
  RestartServer,
  SendHeartbeat,
  ServerUnresponsive,
  StartSupervision
}
import org.enso.projectmanager.util.UnhandledLogging

/**
  * A supervisor process responsible for monitoring language server and
  * restarting it when the server is unresponsive. It delegates server
  * monitoring to the [[HeartbeatSession]] actor.
  *
  * @param config a server config
  * @param server a server handle
  * @param supervisionConfig a supervision config
  * @param connectionFactory a web socket connection factory
  * @param scheduler a scheduler
  */
class LanguageServerSupervisor(
  config: LanguageServerConfig,
  server: LifecycleComponent,
  supervisionConfig: SupervisionConfig,
  connectionFactory: WebSocketConnectionFactory,
  scheduler: Scheduler
) extends Actor
    with ActorLogging
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
      val socket = Socket(config.interface, config.port)
      context.actorOf(
        HeartbeatSession.props(
          socket,
          supervisionConfig.heartbeatTimeout,
          connectionFactory,
          scheduler
        )
      )
      ()

    case ServerUnresponsive =>
      log.info(s"Server is unresponsive [$config]. Restarting it...")
      cancellable.cancel()
      log.info(s"Restarting first time the server")
      server.restart() pipeTo self
      context.become(restarting())

    case GracefulStop =>
      cancellable.cancel()
      stop()
  }

  private def restarting(restartCount: Int = 1): Receive = {
    case RestartServer =>
      log.info(s"Restarting $restartCount time the server")
      server.restart() pipeTo self
      ()

    case Failure(th) =>
      log.error(s"An error occurred during restarting the server [$config]", th)
      if (restartCount < supervisionConfig.numberOfRestarts) {
        scheduler.scheduleOnce(
          supervisionConfig.delayBetweenRestarts,
          self,
          RestartServer
        )
        context.become(restarting(restartCount + 1))
      } else {
        log.error("Cannot restart language server")
        context.parent ! ServerDied
        context.stop(self)
      }

    case ComponentRestarted =>
      log.info(s"Language server restarted [$config]")
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

  private def waitingForChildren(): Receive = {
    case Terminated(_) =>
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

  private case object RestartServer

  /**
    * A command responsible for initiating heartbeat session.
    */
  case object SendHeartbeat

  /**
    * Signals that server is unresponsive.
    */
  case object ServerUnresponsive

  /**
    * Creates a configuration object used to create a [[LanguageServerSupervisor]].
    *
    * @param config a server config
    * @param server a server handle
    * @param supervisionConfig a supervision config
    * @param connectionFactory a web socket connection factory
    * @param scheduler a scheduler
    * @return a configuration object
    */
  def props(
    config: LanguageServerConfig,
    server: LifecycleComponent,
    supervisionConfig: SupervisionConfig,
    connectionFactory: WebSocketConnectionFactory,
    scheduler: Scheduler
  ): Props =
    Props(
      new LanguageServerSupervisor(
        config,
        server,
        supervisionConfig,
        connectionFactory,
        scheduler
      )
    )

}
