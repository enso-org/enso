package org.enso.projectmanager.infrastructure.languageserver

import akka.actor.{
  Actor,
  ActorRef,
  Cancellable,
  OneForOneStrategy,
  Props,
  Stash,
  SupervisorStrategy,
  Terminated
}
import com.typesafe.scalalogging.LazyLogging
import nl.gn0s1s.bump.SemVer
import org.enso.logger.akka.ActorMessageLogging
import org.enso.projectmanager.boot.configuration._
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
import org.enso.projectmanager.service.LoggingServiceDescriptor
import org.enso.projectmanager.util.UnhandledLogging
import org.enso.projectmanager.versionmanagement.DistributionConfiguration

import java.util.UUID

/** A language server controller responsible for managing the server lifecycle.
  * It delegates all tasks to other actors like bootloader or supervisor.
  *
  * @param project a project open by the server
  * @param engineVersion engine version to use for the language server
  * @param bootProgressTracker an [[ActorRef]] that will get progress updates
  *                            related to initializing the engine
  * @param networkConfig a net config
  * @param bootloaderConfig a bootloader config
  * @param supervisionConfig a supervision config
  * @param timeoutConfig a timeout config
  * @param distributionConfiguration configuration of the distribution
  * @param processConfig the configuration of the main process
  * @param loggingServiceDescriptor a logging service configuration descriptor
  * @param executor an executor service used to start the language server
  *                 process
  */
class LanguageServerController(
  project: Project,
  engineVersion: SemVer,
  bootProgressTracker: ActorRef,
  networkConfig: NetworkConfig,
  bootloaderConfig: BootloaderConfig,
  supervisionConfig: SupervisionConfig,
  timeoutConfig: TimeoutConfig,
  distributionConfiguration: DistributionConfiguration,
  processConfig: MainProcessConfig,
  loggingServiceDescriptor: LoggingServiceDescriptor,
  executor: LanguageServerExecutor
) extends Actor
    with LazyLogging
    with ActorMessageLogging
    with Stash
    with UnhandledLogging {

  import context.{dispatcher, system}

  private val descriptor =
    LanguageServerDescriptor(
      name                           = s"language-server-${project.id}",
      rootId                         = UUID.randomUUID(),
      rootPath                       = project.path.toString,
      networkConfig                  = networkConfig,
      distributionConfiguration      = distributionConfiguration,
      engineVersion                  = engineVersion,
      jvmSettings                    = distributionConfiguration.defaultJVMSettings,
      discardOutput                  = distributionConfiguration.shouldDiscardChildOutput,
      profilingEventsLogPath         = processConfig.profilingEventsLogPath,
      profilingPath                  = processConfig.profilingPath,
      profilingTime                  = processConfig.profilingTime,
      deferredLoggingServiceEndpoint = loggingServiceDescriptor.getEndpoint,
      skipGraalVMUpdater             = bootloaderConfig.skipGraalVMUpdater
    )

  override def supervisorStrategy: SupervisorStrategy =
    OneForOneStrategy(10) { case _ =>
      SupervisorStrategy.Restart
    }

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[ClientDisconnected])
    self ! Boot
  }

  override def receive: Receive =
    LoggingReceive.withLabel("initializing") {
      case Boot =>
        val bootloader =
          context.actorOf(
            LanguageServerBootLoader
              .props(
                bootProgressTracker,
                descriptor,
                bootloaderConfig,
                timeoutConfig.bootTimeout,
                executor
              ),
            s"bootloader-${descriptor.name}"
          )
        context.watch(bootloader)
        context.become(booting(bootloader))

      case _ => stash()
    }

  private def booting(Bootloader: ActorRef): Receive =
    LoggingReceive.withLabel("booting") {
      case BootTimeout =>
        logger.error("Booting failed for {}.", descriptor)
        unstashAll()
        context.become(bootFailed(LanguageServerProtocol.ServerBootTimedOut))

      case ServerBootFailed(th) =>
        logger.error("Booting failed for {}. {}", descriptor, th.getMessage)
        unstashAll()
        context.become(bootFailed(LanguageServerProtocol.ServerBootFailed(th)))

      case ServerBooted(connectionInfo, serverProcessManager) =>
        unstashAll()
        context.become(supervising(connectionInfo, serverProcessManager))
        context.actorOf(
          LanguageServerSupervisor.props(
            connectionInfo,
            serverProcessManager,
            supervisionConfig,
            new AkkaBasedWebSocketConnectionFactory(),
            context.system.scheduler
          ),
          s"supervisor-${descriptor.name}"
        )

      case Terminated(Bootloader) =>
        logger.error("Bootloader for project {} failed.", project.name)
        unstashAll()
        context.become(
          bootFailed(
            LanguageServerProtocol.ServerBootFailed(
              new Exception("The number of boot retries exceeded.")
            )
          )
        )

      case _ => stash()
    }

  private def supervising(
    connectionInfo: LanguageServerConnectionInfo,
    serverProcessManager: ActorRef,
    clients: Set[UUID]                     = Set.empty,
    scheduledShutdown: Option[Cancellable] = None
  ): Receive =
    LoggingReceive.withLabel("supervising") {
      case StartServer(clientId, _, requestedEngineVersion, _, _) =>
        scheduledShutdown.foreach(_.cancel())
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
              Socket(connectionInfo.interface, connectionInfo.rpcPort),
              Socket(connectionInfo.interface, connectionInfo.dataPort)
            )
          )
          context.become(
            supervising(
              connectionInfo,
              serverProcessManager,
              clients + clientId,
              None
            )
          )
        }
      case Terminated(_) =>
        scheduledShutdown.foreach(_.cancel())
        logger.debug("Bootloader for {} terminated.", project)

      case StopServer(clientId, _) =>
        removeClient(
          connectionInfo,
          serverProcessManager,
          clients,
          clientId,
          Some(sender()),
          scheduledShutdown
        )

      case ScheduledShutdown(requester) =>
        shutDownServer(requester)

      case ShutDownServer =>
        scheduledShutdown.foreach(_.cancel())
        shutDownServer(None)

      case ClientDisconnected(clientId) =>
        removeClient(
          connectionInfo,
          serverProcessManager,
          clients,
          clientId,
          None,
          scheduledShutdown
        )

      case RenameProject(_, namespace, oldName, newName) =>
        scheduledShutdown.foreach(_.cancel())
        val socket = Socket(connectionInfo.interface, connectionInfo.rpcPort)
        context.actorOf(
          ProjectRenameAction
            .props(
              sender(),
              socket,
              timeoutConfig.requestTimeout,
              timeoutConfig.socketCloseTimeout,
              namespace,
              oldName,
              newName,
              context.system.scheduler
            ),
          s"project-rename-action-${project.id}"
        )

      case ServerDied =>
        scheduledShutdown.foreach(_.cancel())
        logger.error("Language server died [{}].", connectionInfo)
        context.stop(self)

    }

  private def removeClient(
    connectionInfo: LanguageServerConnectionInfo,
    serverProcessManager: ActorRef,
    clients: Set[UUID],
    clientId: UUID,
    maybeRequester: Option[ActorRef],
    shutdownTimeout: Option[Cancellable]
  ): Unit = {
    val updatedClients = clients - clientId
    if (updatedClients.isEmpty) {
      logger.debug("Delaying shutdown for project {}.", project.id)
      val scheduledShutdown =
        shutdownTimeout.orElse(
          Some(
            context.system.scheduler
              .scheduleOnce(
                timeoutConfig.delayedShutdownTimeout,
                self,
                ScheduledShutdown(maybeRequester)
              )
          )
        )
      context.become(
        supervising(
          connectionInfo,
          serverProcessManager,
          Set.empty,
          scheduledShutdown
        )
      )
    } else {
      sender() ! CannotDisconnectOtherClients
      context.become(
        supervising(
          connectionInfo,
          serverProcessManager,
          updatedClients,
          shutdownTimeout
        )
      )
    }
  }

  private def shutDownServer(maybeRequester: Option[ActorRef]): Unit = {
    logger.debug("Shutting down a language server for project {}.", project.id)
    context.children.foreach(_ ! GracefulStop)
    val cancellable =
      context.system.scheduler
        .scheduleOnce(timeoutConfig.shutdownTimeout, self, ShutdownTimeout)
    context.become(stopping(cancellable, maybeRequester))
  }

  private def bootFailed(failure: ServerStartupFailure): Receive = {
    case StartServer(_, _, _, _, _) =>
      sender() ! failure
      stop()
  }

  private def stopping(
    cancellable: Cancellable,
    maybeRequester: Option[ActorRef]
  ): Receive =
    LoggingReceive.withLabel("stopping") {
      case LanguageServerProcess.ServerTerminated(exitCode) =>
        cancellable.cancel()
        if (exitCode == 0) {
          logger.info("Language server shut down successfully [{}].", project)
        } else {
          logger.warn(
            "Language server shut down with non-zero exit code: {} [{}].",
            exitCode,
            project
          )
        }
        maybeRequester.foreach(_ ! ServerStopped)
        stop()

      case ShutdownTimeout =>
        logger.error("Language server shutdown timed out.")
        maybeRequester.foreach(_ ! ServerShutdownTimedOut)
        stop()

      case ClientDisconnected(clientId) =>
        logger.debug(
          s"Received client ($clientId) disconnect request during shutdown. Ignoring."
        )

      case ShutDownServer =>
        logger.debug(s"Received shutdown request during shutdown. Ignoring.")

      case m: StartServer =>
        // This instance has not yet been shut down. Retry
        context.parent.forward(Retry(m))
    }

  private def waitingForChildren(): Receive =
    LoggingReceive.withLabel("waitingForChildren") { case Terminated(_) =>
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
    * @param engineVersion engine version to use for the language server
    * @param bootProgressTracker an [[ActorRef]] that will get progress updates
    *                            related to initializing the engine
    * @param networkConfig a net config
    * @param bootloaderConfig a bootloader config
    * @param supervisionConfig a supervision config
    * @param timeoutConfig a timeout config
    * @param distributionConfiguration configuration of the distribution
    * @param processConfig the configuration of the main process
    * @param loggingServiceDescriptor a logging service configuration descriptor
    * @param executor an executor service used to start the language server
    *                 process
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
    distributionConfiguration: DistributionConfiguration,
    processConfig: MainProcessConfig,
    loggingServiceDescriptor: LoggingServiceDescriptor,
    executor: LanguageServerExecutor
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
        distributionConfiguration,
        processConfig,
        loggingServiceDescriptor,
        executor
      )
    )

  case object ShutDownServer

  /** Signals boot timeout. */
  case object BootTimeout

  /** Boot command. */
  case object Boot

  /** Signals shutdown timeout. */
  case object ShutdownTimeout

  case object ServerDied

  case class Retry(message: Any)

  /** Signals that a (cancellable) shutdown has been initiated */
  case class ScheduledShutdown(requester: Option[ActorRef])

}
