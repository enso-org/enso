package org.enso.projectmanager.infrastructure.languageserver

import java.util.UUID

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Terminated}
import org.enso.projectmanager.boot.configuration.{
  BootloaderConfig,
  NetworkConfig,
  SupervisionConfig
}
import org.enso.projectmanager.infrastructure.languageserver.LanguageServerProtocol.{
  CheckIfServerIsRunning,
  ServerNotRunning,
  StartServer,
  StopServer
}
import org.enso.projectmanager.infrastructure.languageserver.LanguageServerRegistry.ServerShutDown
import org.enso.projectmanager.util.UnhandledLogging

/**
  * An actor that routes request regarding lang. server lifecycle to the
  * right controller that manages the server.
  * It creates a controller actor, if a server doesn't exists.
  *
  * @param networkConfig a net config
  * @param bootloaderConfig a bootloader config
  */
class LanguageServerRegistry(
  networkConfig: NetworkConfig,
  bootloaderConfig: BootloaderConfig,
  supervisionConfig: SupervisionConfig
) extends Actor
    with ActorLogging
    with UnhandledLogging {

  override def receive: Receive = running()

  private def running(
    serverControllers: Map[UUID, ActorRef] = Map.empty
  ): Receive = {
    case msg @ StartServer(_, project) =>
      if (serverControllers.contains(project.id)) {
        serverControllers(project.id).forward(msg)
      } else {
        val controller = context.actorOf(
          LanguageServerController
            .props(project, networkConfig, bootloaderConfig, supervisionConfig)
        )
        context.watch(controller)
        controller.forward(msg)
        context.become(running(serverControllers + (project.id -> controller)))
      }

    case msg @ StopServer(_, projectId) =>
      if (serverControllers.contains(projectId)) {
        serverControllers(projectId).forward(msg)
      } else {
        sender() ! ServerNotRunning
      }

    case ServerShutDown(projectId) =>
      context.become(running(serverControllers - projectId))

    case Terminated(ref) =>
      context.become(running(serverControllers.filterNot(_._2 == ref)))

    case CheckIfServerIsRunning(projectId) =>
      sender() ! serverControllers.contains(projectId)

  }

}

object LanguageServerRegistry {

  /**
    * A notification informing that a server has shut down.
    *
    * @param projectId a project id
    */
  case class ServerShutDown(projectId: UUID)

  /**
    *  Creates a configuration object used to create a [[LanguageServerRegistry]].
    *
    * @param networkConfig a net config
    * @param bootloaderConfig a bootloader config
    * @return
    */
  def props(
    networkConfig: NetworkConfig,
    bootloaderConfig: BootloaderConfig,
    supervisionConfig: SupervisionConfig
  ): Props =
    Props(
      new LanguageServerRegistry(
        networkConfig,
        bootloaderConfig,
        supervisionConfig
      )
    )

}
