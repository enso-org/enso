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
  SupervisionConfig
}
import org.enso.projectmanager.data.Socket
import org.enso.projectmanager.event.ClientEvent.ClientDisconnected
import org.enso.projectmanager.infrastructure.http.AkkaBasedWebSocketConnectionFactory
import org.enso.projectmanager.infrastructure.languageserver.LanguageServerBootLoader.{
  ServerBootFailed,
  ServerBooted
}
import org.enso.projectmanager.infrastructure.languageserver.LanguageServerController.{
  Boot,
  BootTimeout,
  ServerDied
}
import org.enso.projectmanager.infrastructure.languageserver.LanguageServerProtocol._
import org.enso.projectmanager.infrastructure.languageserver.LanguageServerRegistry.ServerShutDown
import org.enso.projectmanager.model.Project
import org.enso.projectmanager.util.UnhandledLogging

import scala.concurrent.duration._

/**
  * A language server controller responsible for managing the server lifecycle.
  * It delegates all tasks to other actors like bootloader or supervisor.
  *
  * @param project a project open by the server
  * @param networkConfig a net config
  * @param bootloaderConfig a bootloader config
  */
class LanguageServerController(
  project: Project,
  networkConfig: NetworkConfig,
  bootloaderConfig: BootloaderConfig,
  supervisionConfig: SupervisionConfig
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

  override def supervisorStrategy: SupervisorStrategy = OneForOneStrategy(10) {
    case _ => SupervisorStrategy.Restart
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
      stop()

    case ServerBootFailed(th) =>
      unstashAll()
      timeoutCancellable.cancel()
      context.become(bootFailed(th))

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
        bootFailed(new Exception("The number of boot retries exceeded"))
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
        Socket(config.interface, config.port)
      )
      context.become(supervising(config, server, clients + clientId))

    case Terminated(_) =>
      log.debug(s"Bootloader for $project terminated.")

    case StopServer(clientId, _) =>
      removeClient(config, server, clients, clientId, Some(sender()))

    case ClientDisconnected(clientId) =>
      removeClient(config, server, clients, clientId, None)

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
      server.stop() pipeTo self
      context.become(stopping(maybeRequester))
    } else {
      sender() ! CannotDisconnectOtherClients
      context.become(supervising(config, server, updatedClients))
    }
  }

  private def bootFailed(th: Throwable): Receive = {
    case StartServer(_, _) =>
      sender() ! LanguageServerProtocol.ServerBootFailed(th)
      stop()
  }

  private def stopping(maybeRequester: Option[ActorRef]): Receive = {
    case Failure(th) =>
      log.error(
        th,
        s"An error occurred during Language server shutdown [$project]."
      )
      maybeRequester.foreach(_ ! FailureDuringStoppage(th))
      stop()

    case ComponentStopped =>
      log.info(s"Language server shut down successfully [$project].")
      maybeRequester.foreach(_ ! ServerStopped)
      stop()
  }

  private def waitingForChildren(): Receive = {
    case Terminated(_) =>
      if (context.children.isEmpty) {
        context.stop(self)
      }
  }

  private def stop(): Unit = {
    context.parent ! ServerShutDown(project.id)
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

  /**
    * Creates a configuration object used to create a [[LanguageServerController]].
    *
    * @param project a project open by the server
    * @param networkConfig a net config
    * @param bootloaderConfig a bootloader config
    * @return a configuration object
    */
  def props(
    project: Project,
    networkConfig: NetworkConfig,
    bootloaderConfig: BootloaderConfig,
    supervisionConfig: SupervisionConfig
  ): Props =
    Props(
      new LanguageServerController(
        project,
        networkConfig,
        bootloaderConfig,
        supervisionConfig
      )
    )

  /**
    * Signals boot timeout.
    */
  case object BootTimeout

  /**
    * Boot command.
    */
  case object Boot

  case object ServerDied

}
