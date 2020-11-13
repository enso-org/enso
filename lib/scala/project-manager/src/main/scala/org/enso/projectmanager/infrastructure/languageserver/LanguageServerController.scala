package org.enso.projectmanager.infrastructure.languageserver

import java.util.UUID

import akka.actor.Status.Failure
import akka.actor.{
  Actor,
  ActorLogging,
  ActorRef,
  Cancellable,
  OneForOneStrategy,
  Props,
  Stash,
  SupervisorStrategy,
  Terminated
}
import akka.pattern.pipe
import org.enso.languageserver.boot.LifecycleComponent.ComponentStopped
import org.enso.languageserver.boot.{
  LanguageServerComponent,
  LanguageServerConfig
}
import org.enso.projectmanager.boot.configuration.{
  BootloaderConfig,
  NetworkConfig,
  SupervisionConfig,
  TimeoutConfig
}
import org.enso.projectmanager.data.{LanguageServerSockets, Socket}
import org.enso.projectmanager.event.ClientEvent.ClientDisconnected
import org.enso.projectmanager.event.ProjectEvent.ProjectClosed
import org.enso.projectmanager.infrastructure.http.AkkaBasedWebSocketConnectionFactory
import org.enso.projectmanager.infrastructure.languageserver.LanguageServerBootLoader.{
  ServerBootFailed,
  ServerBooted
}
import org.enso.projectmanager.infrastructure.languageserver.LanguageServerController.{
  Boot,
  BootTimeout,
  ServerDied,
  ShutDownServer,
  ShutdownTimeout
}
import org.enso.projectmanager.infrastructure.languageserver.LanguageServerProtocol._
import org.enso.projectmanager.infrastructure.languageserver.LanguageServerRegistry.ServerShutDown
import org.enso.projectmanager.model.Project
import org.enso.projectmanager.util.UnhandledLogging

import scala.concurrent.duration._

/** A language server controller responsible for managing the server lifecycle.
  * It delegates all tasks to other actors like bootloader or supervisor.
  *
  * @param project a project open by the server
  * @param networkConfig a net config
  * @param bootloaderConfig a bootloader config
  * @param supervisionConfig a supervision config
  * @param timeoutConfig a timeout config
  */
class LanguageServerController(
  project: Project,
  networkConfig: NetworkConfig,
  bootloaderConfig: BootloaderConfig,
  supervisionConfig: SupervisionConfig,
  timeoutConfig: TimeoutConfig
) extends Actor
    with ActorLogging
    with Stash
    with UnhandledLogging {

  import context.{dispatcher, system}

  private val descriptor =
    LanguageServerDescriptor(
      name          = s"language-server-${project.id}",
      rootId        = UUID.randomUUID(),
      root          = project.path.get,
      networkConfig = networkConfig
    )

  override def supervisorStrategy: SupervisorStrategy =
    OneForOneStrategy(10) { case _ =>
      SupervisorStrategy.Restart
    }

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[ClientDisconnected])
    self ! Boot
  }

  override def receive: Receive = {
    case Boot =>
      val bootloader =
        context.actorOf(
          LanguageServerBootLoader.props(descriptor, bootloaderConfig),
          "bootloader"
        )
      context.watch(bootloader)
      val timeoutCancellable =
        context.system.scheduler.scheduleOnce(30.seconds, self, BootTimeout)
      context.become(booting(bootloader, timeoutCancellable))

    case _ => stash()
  }

  private def booting(
    Bootloader: ActorRef,
    timeoutCancellable: Cancellable
  ): Receive = {
    case BootTimeout =>
      log.error(s"Booting failed for $descriptor")
      unstashAll()
      context.become(bootFailed(LanguageServerProtocol.ServerBootTimedOut))

    case ServerBootFailed(th) =>
      log.error(th, s"Booting failed for $descriptor")
      unstashAll()
      timeoutCancellable.cancel()
      context.become(bootFailed(LanguageServerProtocol.ServerBootFailed(th)))

    case ServerBooted(config, server) =>
      unstashAll()
      timeoutCancellable.cancel()
      context.become(supervising(config, server))
      context.actorOf(
        LanguageServerSupervisor.props(
          config,
          server,
          supervisionConfig,
          new AkkaBasedWebSocketConnectionFactory(),
          context.system.scheduler
        ),
        "supervisor"
      )

    case Terminated(Bootloader) =>
      log.error(s"Bootloader for project ${project.name} failed")
      unstashAll()
      timeoutCancellable.cancel()
      context.become(
        bootFailed(
          LanguageServerProtocol.ServerBootFailed(
            new Exception("The number of boot retries exceeded")
          )
        )
      )

    case _ => stash()
  }

  private def supervising(
    config: LanguageServerConfig,
    server: LanguageServerComponent,
    clients: Set[UUID] = Set.empty
  ): Receive = {
    case StartServer(clientId, _) =>
      sender() ! ServerStarted(
        LanguageServerSockets(
          Socket(config.interface, config.rpcPort),
          Socket(config.interface, config.dataPort)
        )
      )
      context.become(supervising(config, server, clients + clientId))

    case Terminated(_) =>
      log.debug(s"Bootloader for $project terminated.")

    case StopServer(clientId, _) =>
      removeClient(config, server, clients, clientId, Some(sender()))

    case ShutDownServer =>
      shutDownServer(server, None)

    case ClientDisconnected(clientId) =>
      removeClient(config, server, clients, clientId, None)

    case RenameProject(_, oldName, newName) =>
      val socket = Socket(config.interface, config.rpcPort)
      context.actorOf(
        ProjectRenameAction
          .props(
            sender(),
            socket,
            timeoutConfig.requestTimeout,
            timeoutConfig.socketCloseTimeout,
            oldName,
            newName,
            context.system.scheduler
          ),
        s"project-rename-action-${project.id}"
      )

    case ServerDied =>
      log.error(s"Language server died [$config]")
      context.stop(self)
  }

  private def removeClient(
    config: LanguageServerConfig,
    server: LanguageServerComponent,
    clients: Set[UUID],
    clientId: UUID,
    maybeRequester: Option[ActorRef]
  ): Unit = {
    val updatedClients = clients - clientId
    if (updatedClients.isEmpty) {
      shutDownServer(server, maybeRequester)
    } else {
      sender() ! CannotDisconnectOtherClients
      context.become(supervising(config, server, updatedClients))
    }
  }

  private def shutDownServer(
    server: LanguageServerComponent,
    maybeRequester: Option[ActorRef]
  ): Unit = {
    log.debug(s"Shutting down a language server for project ${project.id}")
    context.children.foreach(_ ! GracefulStop)
    server.stop() pipeTo self
    val cancellable =
      context.system.scheduler
        .scheduleOnce(timeoutConfig.shutdownTimeout, self, ShutdownTimeout)
    context.become(stopping(cancellable, maybeRequester))
  }

  private def bootFailed(failure: ServerStartupFailure): Receive = {
    case StartServer(_, _) =>
      sender() ! failure
      stop()
  }

  private def stopping(
    cancellable: Cancellable,
    maybeRequester: Option[ActorRef]
  ): Receive = {
    case Failure(th) =>
      cancellable.cancel()
      log.error(
        th,
        s"An error occurred during Language server shutdown [$project]."
      )
      maybeRequester.foreach(_ ! FailureDuringShutdown(th))
      stop()

    case ComponentStopped =>
      cancellable.cancel()
      log.info(s"Language server shut down successfully [$project].")
      maybeRequester.foreach(_ ! ServerStopped)
      stop()

    case ShutdownTimeout =>
      log.error("Language server shutdown timed out")
      maybeRequester.foreach(_ ! ServerShutdownTimedOut)
      stop()

    case StartServer(_, _) =>
      sender() ! PreviousInstanceNotShutDown
  }

  private def waitingForChildren(): Receive = { case Terminated(_) =>
    if (context.children.isEmpty) {
      context.stop(self)
    }
  }

  private def stop(): Unit = {
    context.parent ! ServerShutDown(project.id)
    context.system.eventStream.publish(ProjectClosed(project.id))
    if (context.children.isEmpty) {
      context.stop(self)
    } else {
      context.children.foreach(_ ! GracefulStop)
      context.children.foreach(context.watch)
      context.become(waitingForChildren())
    }
  }

}

object LanguageServerController {

  /** Creates a configuration object used to create a [[LanguageServerController]].
    *
    * @param project a project open by the server
    * @param networkConfig a net config
    * @param bootloaderConfig a bootloader config
    * @param supervisionConfig a supervision config
    * @param timeoutConfig a timeout config
    * @return a configuration object
    */
  def props(
    project: Project,
    networkConfig: NetworkConfig,
    bootloaderConfig: BootloaderConfig,
    supervisionConfig: SupervisionConfig,
    timeoutConfig: TimeoutConfig
  ): Props =
    Props(
      new LanguageServerController(
        project,
        networkConfig,
        bootloaderConfig,
        supervisionConfig,
        timeoutConfig
      )
    )

  case object ShutDownServer

  /** Signals boot timeout.
    */
  case object BootTimeout

  /** Boot command.
    */
  case object Boot

  /** Signals shutdown timeout.
    */
  case object ShutdownTimeout

  case object ServerDied

}
