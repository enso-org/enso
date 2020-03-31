package org.enso.projectmanager.protocol

import java.util.UUID

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Stash}
import org.enso.jsonrpc.{JsonRpcServer, MessageHandler, Method, Request}
import org.enso.projectmanager.boot.configuration.TimeoutConfig
import org.enso.projectmanager.control.effect.Exec
import org.enso.projectmanager.event.ClientEvent.{
  ClientConnected,
  ClientDisconnected
}
import org.enso.projectmanager.protocol.ProjectManagementApi.{
  ProjectClose,
  ProjectCreate,
  ProjectDelete,
  ProjectOpen
}
import org.enso.projectmanager.requesthandler.{
  ProjectCloseHandler,
  ProjectCreateHandler,
  ProjectDeleteHandler,
  ProjectOpenHandler
}
import org.enso.projectmanager.service.ProjectServiceApi

/**
  * An actor handling communications between a single client and the project
  * manager.
  *
  * @param clientId the internal client id.
  * @param projectService a project service
  * @param config a request timeout cofig
  */
class ClientController[F[+_, +_]: Exec](
  clientId: UUID,
  projectService: ProjectServiceApi[F],
  config: TimeoutConfig
) extends Actor
    with Stash
    with ActorLogging {

  private val requestHandlers: Map[Method, Props] =
    Map(
      ProjectCreate -> ProjectCreateHandler
        .props[F](projectService, config.requestTimeout),
      ProjectDelete -> ProjectDeleteHandler
        .props[F](projectService, config.requestTimeout),
      ProjectOpen -> ProjectOpenHandler
        .props[F](clientId, projectService, config.bootTimeout),
      ProjectClose -> ProjectCloseHandler
        .props[F](clientId, projectService, config.bootTimeout)
    )

  override def unhandled(message: Any): Unit =
    log.warning("Received unknown message: {}", message)

  override def receive: Receive = {
    case JsonRpcServer.WebConnect(webActor) =>
      log.info(s"Client connected to Project Manager [$clientId]")
      unstashAll()
      context.become(connected(webActor))
      context.system.eventStream.publish(ClientConnected(clientId))

    case _ => stash()
  }

  def connected(webActor: ActorRef): Receive = {
    case MessageHandler.Disconnected =>
      log.info(s"Client disconnected from the Project Manager [$clientId]")
      context.system.eventStream.publish(ClientDisconnected(clientId))
      context.stop(self)

    case r @ Request(method, _, _) if (requestHandlers.contains(method)) =>
      val handler = context.actorOf(requestHandlers(method))
      handler.forward(r)
  }
}

object ClientController {

  /**
    * Creates a configuration object used to create a [[ClientController]].
    *
    * @param clientId the internal client id.
    * @return a configuration object
    */
  def props[F[+_, +_]: Exec](
    clientId: UUID,
    projectService: ProjectServiceApi[F],
    config: TimeoutConfig
  ): Props =
    Props(new ClientController(clientId, projectService, config: TimeoutConfig))

}
