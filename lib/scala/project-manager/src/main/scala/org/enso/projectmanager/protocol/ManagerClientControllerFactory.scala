package org.enso.projectmanager.protocol

import java.util.UUID

import akka.actor.{ActorRef, ActorSystem}
import org.enso.jsonrpc.ClientControllerFactory
import org.enso.projectmanager.boot.configuration.TimeoutConfig
import org.enso.projectmanager.control.core.CovariantFlatMap
import org.enso.projectmanager.control.effect.{ErrorChannel, Exec}
import org.enso.projectmanager.service.config.GlobalConfigServiceApi
import org.enso.projectmanager.service.versionmanagement.RuntimeVersionManagementServiceApi
import org.enso.projectmanager.service.{
  LoggingServiceDescriptor,
  ProjectServiceApi
}

/** Project manager client controller factory.
  *
  * @param system the actor system
  * @param projectService a project service
  * @param globalConfigService global configuration service
  * @param runtimeVersionManagementService version management service
  * @param loggingServiceDescriptor a logging service configuration descriptor
  * @param timeoutConfig a request timeout config
  */
class ManagerClientControllerFactory[
  F[+_, +_]: Exec: CovariantFlatMap: ErrorChannel
](
  system: ActorSystem,
  projectService: ProjectServiceApi[F],
  globalConfigService: GlobalConfigServiceApi[F],
  runtimeVersionManagementService: RuntimeVersionManagementServiceApi[F],
  loggingServiceDescriptor: LoggingServiceDescriptor,
  timeoutConfig: TimeoutConfig
) extends ClientControllerFactory {

  /** Creates a client controller actor.
    *
    * @param clientId the internal client id.
    * @return an actor ref to the client controller
    */
  override def createClientController(clientId: UUID): ActorRef =
    system.actorOf(
      ClientController
        .props[F](
          clientId,
          projectService,
          globalConfigService,
          runtimeVersionManagementService,
          loggingServiceDescriptor,
          timeoutConfig
        ),
      s"manager-client-controller-$clientId"
    )

}
