package org.enso.projectmanager.protocol

import java.util.UUID

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Stash}
import org.enso.jsonrpc.{JsonRpcServer, MessageHandler, Method, Request}
import org.enso.projectmanager.control.effect.Exec
import org.enso.projectmanager.protocol.ProjectManagementApi.{
  ProjectCreate,
  ProjectDelete
}
import org.enso.projectmanager.requesthandler.{
  ProjectCreateHandler,
  ProjectDeleteHandler
}
import org.enso.projectmanager.service.ProjectServiceApi

import scala.concurrent.duration.FiniteDuration

/**
  * An actor handling communications between a single client and the project
  * manager.
  *
  * @param clientId the internal client id.
  * @param projectService a project service
  * @param timeout a request timeout
  */
class ClientController[F[+_, +_]: Exec](
  clientId: UUID,
  projectService: ProjectServiceApi[F],
  timeout: FiniteDuration
) extends Actor
    with Stash
    with ActorLogging {

  private val requestHandlers: Map[Method, Props] =
    Map(
      ProjectCreate -> ProjectCreateHandler.props[F](projectService, timeout),
      ProjectDelete -> ProjectDeleteHandler.props[F](projectService, timeout)
    )

  override def unhandled(message: Any): Unit =
    log.warning("Received unknown message: {}", message)

  override def receive: Receive = {
    case JsonRpcServer.WebConnect(webActor) =>
      unstashAll()
      context.become(connected(webActor))

    case _ => stash()
  }

  def connected(webActor: ActorRef): Receive = {
    case MessageHandler.Disconnected =>
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
    timeout: FiniteDuration
  ): Props =
    Props(new ClientController(clientId, projectService, timeout))

}
