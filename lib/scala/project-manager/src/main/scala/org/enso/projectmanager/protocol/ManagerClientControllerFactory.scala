package org.enso.projectmanager.protocol

import java.util.UUID

import akka.actor.{ActorRef, ActorSystem}
import org.enso.jsonrpc.ClientControllerFactory
import org.enso.projectmanager.boot.configuration.TimeoutConfig
import org.enso.projectmanager.control.effect.Exec
import org.enso.projectmanager.service.ProjectServiceApi

/** Project manager client controller factory.
  *
  * @param system the actor system
  */
class ManagerClientControllerFactory[F[+_, +_]: Exec](
  system: ActorSystem,
  projectService: ProjectServiceApi[F],
  timeoutConfig: TimeoutConfig
) extends ClientControllerFactory {

  /** Creates a client controller actor.
    *
    * @param clientId the internal client id.
    * @return an actor ref to the client controller
    */
  override def createClientController(clientId: UUID): ActorRef =
    system.actorOf(
      ClientController.props[F](clientId, projectService, timeoutConfig),
      s"jsonrpc-connection-controller-$clientId"
    )

}
