package org.enso.projectmanager.infrastructure.languageserver

import java.util.UUID

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
import nl.gn0s1s.bump.SemVer
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
import org.enso.projectmanager.infrastructure.languageserver.LanguageServerController._
import org.enso.projectmanager.infrastructure.languageserver.LanguageServerProtocol._
import org.enso.projectmanager.infrastructure.languageserver.LanguageServerRegistry.ServerShutDown
import org.enso.projectmanager.model.Project
import org.enso.projectmanager.util.UnhandledLogging
import org.enso.projectmanager.versionmanagement.DistributionConfiguration
import org.enso.runtimeversionmanager.runner.JVMSettings

/** A language server controller responsible for managing the server lifecycle.
  * It delegates all tasks to other actors like bootloader or supervisor.
  *
  * @param project a project open by the server
  * @param engineVersion
  * @param bootProgressTracker
  * @param networkConfig a net config
  * @param bootloaderConfig a bootloader config
  * @param supervisionConfig a supervision config
  * @param timeoutConfig a timeout config
  * @param distributionConfiguration
  */
class LanguageServerController(
  project: Project,
  engineVersion: SemVer,
  bootProgressTracker: ActorRef,
  networkConfig: NetworkConfig,
  bootloaderConfig: BootloaderConfig,
  supervisionConfig: SupervisionConfig,
  timeoutConfig: TimeoutConfig,
  distributionConfiguration: DistributionConfiguration
) extends Actor
    with ActorLogging
    with Stash
    with UnhandledLogging {

  import context.{dispatcher, system}

  private val descriptor =
    LanguageServerDescriptor(
      name                      = s"language-server-${project.id}",
      rootId                    = UUID.randomUUID(),
      rootPath                  = project.path.get,
      networkConfig             = networkConfig,
      distributionConfiguration = distributionConfiguration,
      engineVersion             = engineVersion,
      jvmSettings               = JVMSettings.default // TODO allow overriding
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
          LanguageServerBootLoader
            .props(
              bootProgressTracker,
              descriptor,
              bootloaderConfig,
              timeoutConfig.bootTimeout
            ),
          s"bootloader-${descriptor.name}"
        )
      context.watch(bootloader)
      context.become(booting(bootloader))

    case _ => stash()
  }

  private def booting(Bootloader: ActorRef): Receive = {
    case BootTimeout =>
      log.error(s"Booting failed for $descriptor")
      unstashAll()
      context.become(bootFailed(LanguageServerProtocol.ServerBootTimedOut))

    case ServerBootFailed(th) =>
      log.error(th, s"Booting failed for $descriptor")
      unstashAll()
      context.become(bootFailed(LanguageServerProtocol.ServerBootFailed(th)))

    case ServerBooted(connectionInfo, server) =>
      unstashAll()
      context.become(supervising(connectionInfo, server))
      context.actorOf(
        LanguageServerSupervisor.props(
          connectionInfo,
          server,
          supervisionConfig,
          new AkkaBasedWebSocketConnectionFactory(),
          context.system.scheduler
        ),
        s"supervisor-${descriptor.name}"
      )

    case Terminated(Bootloader) =>
      log.error(s"Bootloader for project ${project.name} failed")
      unstashAll()
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
    config: LanguageServerConnectionInfo,
    serverProcess: ActorRef,
    clients: Set[UUID] = Set.empty
  ): Receive = {
    case StartServer(clientId, _, requestedEngineVersion, _) =>
      if (requestedEngineVersion != engineVersion) {
        sender() ! ServerBootFailed(
          new IllegalStateException(
            s"Requested to boot a server version $requestedEngineVersion, " +
            s"but a server for this project with a different version, " +
            s"$engineVersion, is already running. Two servers with different " +
            s"versions cannot be running for a single project."
          )
        )
      } else {
        sender() ! ServerStarted(
          LanguageServerSockets(
            Socket(config.interface, config.rpcPort),
            Socket(config.interface, config.dataPort)
          )
        )
        context.become(supervising(config, serverProcess, clients + clientId))
      }
    case Terminated(_) =>
      log.debug(s"Bootloader for $project terminated.")

    case StopServer(clientId, _) =>
      removeClient(config, serverProcess, clients, clientId, Some(sender()))

    case ShutDownServer =>
      shutDownServer(serverProcess, None)

    case ClientDisconnected(clientId) =>
      removeClient(config, serverProcess, clients, clientId, None)

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
    config: LanguageServerConnectionInfo,
    serverProcess: ActorRef,
    clients: Set[UUID],
    clientId: UUID,
    maybeRequester: Option[ActorRef]
  ): Unit = {
    val updatedClients = clients - clientId
    if (updatedClients.isEmpty) {
      shutDownServer(serverProcess, maybeRequester)
    } else {
      sender() ! CannotDisconnectOtherClients
      context.become(supervising(config, serverProcess, updatedClients))
    }
  }

  private def shutDownServer(
    serverProcess: ActorRef,
    maybeRequester: Option[ActorRef]
  ): Unit = {
    log.debug(s"Shutting down a language server for project ${project.id}")
    context.children.foreach(_ ! GracefulStop)
    serverProcess ! LanguageServerProcess.Stop
    val cancellable =
      context.system.scheduler
        .scheduleOnce(timeoutConfig.shutdownTimeout, self, ShutdownTimeout)
    context.become(stopping(cancellable, maybeRequester))
  }

  private def bootFailed(failure: ServerStartupFailure): Receive = {
    case StartServer(_, _, _, _) =>
      sender() ! failure
      stop()
  }

  private def stopping(
    cancellable: Cancellable,
    maybeRequester: Option[ActorRef]
  ): Receive = {
    case LanguageServerProcess.ServerTerminated(exitCode) =>
      cancellable.cancel()
      if (exitCode == 0) {
        log.info(s"Language server shut down successfully [$project].")
      } else {
        log.warning(
          s"Language server shut down with non-zero exit code: $exitCode " +
          s"[$project]."
        )
      }
      maybeRequester.foreach(_ ! ServerStopped)
      stop()

    case ShutdownTimeout =>
      log.error("Language server shutdown timed out")
      maybeRequester.foreach(_ ! ServerShutdownTimedOut)
      stop()

    case StartServer(_, _, _, _) =>
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
    * @param engineVersion
    * @param bootProgressTracker
    * @param networkConfig a net config
    * @param bootloaderConfig a bootloader config
    * @param supervisionConfig a supervision config
    * @param timeoutConfig a timeout config
    * @param distributionConfiguration
    * @return a configuration object
    */
  def props(
    project: Project,
    engineVersion: SemVer,
    bootProgressTracker: ActorRef,
    networkConfig: NetworkConfig,
    bootloaderConfig: BootloaderConfig,
    supervisionConfig: SupervisionConfig,
    timeoutConfig: TimeoutConfig,
    distributionConfiguration: DistributionConfiguration
  ): Props =
    Props(
      new LanguageServerController(
        project,
        engineVersion,
        bootProgressTracker,
        networkConfig,
        bootloaderConfig,
        supervisionConfig,
        timeoutConfig,
        distributionConfiguration
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
