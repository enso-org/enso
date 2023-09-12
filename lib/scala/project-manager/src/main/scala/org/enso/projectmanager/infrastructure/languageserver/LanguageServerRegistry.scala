package org.enso.projectmanager.infrastructure.languageserver

import akka.actor.{Actor, ActorRef, Cancellable, Props, Terminated}
import com.typesafe.scalalogging.LazyLogging
import org.enso.projectmanager.boot.configuration._
import org.enso.projectmanager.infrastructure.languageserver.LanguageServerController.Retry
import org.enso.projectmanager.infrastructure.languageserver.LanguageServerProtocol._
import org.enso.projectmanager.service.LoggingServiceDescriptor
import org.enso.projectmanager.util.UnhandledLogging
import org.enso.projectmanager.versionmanagement.DistributionConfiguration

import java.util.UUID
import scala.concurrent.duration._

/** An actor that routes request regarding lang. server lifecycle to the
  * right controller that manages the server.
  * It creates a controller actor, if a server doesn't exist.
  *
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
class LanguageServerRegistry(
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
    with UnhandledLogging {

  import LanguageServerRegistry._
  import context.dispatcher

  override def receive: Receive = running()

  private def running(
    serverControllers: Map[UUID, ActorRef]                  = Map.empty,
    pendingReplies: Map[UUID, (Seq[ActorRef], Cancellable)] = Map.empty
  ): Receive = {
    case msg @ StartServer(
          _,
          project,
          engineVersion,
          progressTracker,
          engineUpdate
        ) =>
      if (serverControllers.contains(project.id)) {
        serverControllers(project.id).forward(msg)
      } else {
        val controller = context.actorOf(
          LanguageServerController
            .props(
              project,
              engineVersion,
              progressTracker,
              networkConfig,
              bootloaderConfig.copy(skipGraalVMUpdater = engineUpdate),
              supervisionConfig,
              timeoutConfig,
              distributionConfiguration,
              processConfig,
              loggingServiceDescriptor,
              executor
            ),
          s"language-server-controller-${project.id}"
        )
        context.watch(controller)
        controller.forward(msg)
        context.become(
          running(
            serverControllers + (project.id -> controller),
            pendingReplies
          )
        )
      }

    case msg @ StopServer(_, projectId) =>
      if (serverControllers.contains(projectId)) {
        serverControllers(projectId).forward(msg)
      } else {
        sender() ! ServerNotRunning
      }

    case msg @ KillThemAll =>
      val killer = context.actorOf(
        LanguageServerKiller.props(
          serverControllers,
          timeoutConfig.shutdownTimeout
        ),
        "language-server-killer"
      )
      killer.forward(msg)
      context.become(running(pendingReplies = pendingReplies))

    case ServerShutDown(projectId) =>
      context.become(running(serverControllers - projectId))

    case msg @ RenameProject(projectId, _, _, _) =>
      if (serverControllers.contains(projectId)) {
        serverControllers(projectId).forward(msg)
      } else {
        sender() ! ProjectNotOpened
      }

    case Terminated(ref) =>
      val pending = serverControllers
        .find(_._2 == ref)
        .map(e => pendingReplies.filterNot(_._1 == e._1))
        .getOrElse(pendingReplies)
      context.become(running(serverControllers.filterNot(_._2 == ref), pending))

    case CheckIfServerIsRunning(projectId) =>
      serverControllers.get(projectId) match {
        case Some(ref) =>
          val replyTo = sender()
          pendingReplies.get(projectId) match {
            case Some((prevRefs, cancellable)) if !prevRefs.contains(replyTo) =>
              context.become(
                running(
                  serverControllers,
                  pendingReplies + (
                    (
                      projectId,
                      (replyTo +: prevRefs, cancellable)
                    )
                  )
                )
              )
            case None =>
              val scheduledRequest =
                context.system.scheduler.scheduleOnce(
                  timeoutConfig.requestTimeout,
                  self,
                  LanguageServerStatusTimeout(projectId)
                )
              ref ! LanguageServerStatusRequest
              context.become(
                running(
                  serverControllers,
                  pendingReplies + (
                    (
                      projectId,
                      (Seq(replyTo), scheduledRequest)
                    )
                  )
                )
              )
            case _ =>
            // Do nothing, still waiting
          }
        case None =>
          sender() ! (false, false)
      }

    case LanguageServerStatus(uuid, shuttingDown) =>
      pendingReplies.get(uuid) match {
        case Some((replyTo, cancellable)) =>
          cancellable.cancel()
          replyTo.foreach(_ ! (true, shuttingDown))
          context.become(
            running(serverControllers, pendingReplies.filterNot(_._1 == uuid))
          )
        case None =>
          logger.warn(
            "Unknown request for language server state for project {}",
            uuid
          )
      }

    case LanguageServerStatusTimeout(uuid) =>
      pendingReplies.get(uuid) match {
        case Some((replyTo, _)) =>
          replyTo.foreach(_ ! CheckTimeout)
          context.become(
            running(serverControllers, pendingReplies.filterNot(_._1 == uuid))
          )
        case None =>
          logger.warn(
            "Unknown request for language server status for project {}",
            uuid
          )
      }

    case Retry(message) =>
      context.system.scheduler.scheduleOnce(200.millis, self, message)(
        context.dispatcher,
        context.sender()
      )
  }

}

object LanguageServerRegistry {

  /** A notification informing that a server has shut down.
    *
    * @param projectId a project id
    */
  case class ServerShutDown(projectId: UUID)

  /**  Creates a configuration object used to create a [[LanguageServerRegistry]].
    *
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
      new LanguageServerRegistry(
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

  /** A notification that no project status response has been received within a timeout.
    *
    * @param projectId uuid of a project that its language server failed to report on
    */
  case class LanguageServerStatusTimeout(projectId: UUID)

  /** The state of language server for a given project.
    *
    * @param projectId uuid of the project
    * @param shuttingDown if true, the project is currently in a soft shutdown state, false otherwise
    */
  case class LanguageServerStatus(projectId: UUID, shuttingDown: Boolean)

  /** A message requesting the current state of the language server.
    */
  case object LanguageServerStatusRequest

}
