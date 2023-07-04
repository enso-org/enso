package org.enso.projectmanager.protocol

import java.util.UUID

import akka.actor.{Actor, ActorRef, Props, Stash}
import com.typesafe.scalalogging.LazyLogging
import org.enso.jsonrpc.{JsonRpcServer, MessageHandler, Method, Request}
import org.enso.projectmanager.boot.configuration.TimeoutConfig
import org.enso.projectmanager.control.core.CovariantFlatMap
import org.enso.projectmanager.control.effect.{ErrorChannel, Exec}
import org.enso.projectmanager.event.ClientEvent.{
  ClientConnected,
  ClientDisconnected
}
import org.enso.projectmanager.protocol.ProjectManagementApi._
import org.enso.projectmanager.requesthandler._
import org.enso.projectmanager.service.config.GlobalConfigServiceApi
import org.enso.projectmanager.service.versionmanagement.RuntimeVersionManagementServiceApi
import org.enso.projectmanager.service.{
  LoggingServiceDescriptor,
  ProjectServiceApi
}
import org.enso.projectmanager.util.UnhandledLogging

import scala.annotation.unused
import scala.concurrent.duration._

/** An actor handling communications between a single client and the project
  * manager.
  *
  * @param clientId the internal client id.
  * @param projectService a project service
  * @param globalConfigService global configuration service
  * @param runtimeVersionManagementService version management service
  * @param loggingServiceDescriptor a logging service configuration descriptor
  * @param timeoutConfig a request timeout config
  */
class ClientController[F[+_, +_]: Exec: CovariantFlatMap: ErrorChannel](
  clientId: UUID,
  projectService: ProjectServiceApi[F],
  globalConfigService: GlobalConfigServiceApi[F],
  runtimeVersionManagementService: RuntimeVersionManagementServiceApi[F],
  loggingServiceDescriptor: LoggingServiceDescriptor,
  timeoutConfig: TimeoutConfig
) extends Actor
    with LazyLogging
    with Stash
    with UnhandledLogging {

  private val requestHandlers: Map[Method, Props] =
    Map(
      ProjectCreate -> ProjectCreateHandler
        .props[F](
          globalConfigService,
          projectService,
          timeoutConfig.requestTimeout,
          timeoutConfig.retries
        ),
      ProjectDelete -> ProjectDeleteHandler
        .props[F](projectService, timeoutConfig.requestTimeout),
      ProjectOpen -> ProjectOpenHandler
        .props[F](
          clientId,
          projectService,
          timeoutConfig.bootTimeout,
          timeoutConfig.retries
        ),
      ProjectClose -> ProjectCloseHandler
        .props[F](
          clientId,
          projectService,
          timeoutConfig.shutdownTimeout.plus(1.second)
        ),
      ProjectList -> ProjectListHandler
        .props[F](
          projectService,
          timeoutConfig.requestTimeout,
          timeoutConfig.retries
        ),
      ProjectRename -> ProjectRenameHandler
        .props[F](projectService, timeoutConfig.requestTimeout),
      EngineListInstalled -> EngineListInstalledHandler.props(
        runtimeVersionManagementService,
        timeoutConfig.requestTimeout
      ),
      EngineListAvailable -> EngineListAvailableHandler.props(
        runtimeVersionManagementService,
        timeoutConfig.requestTimeout
      ),
      EngineInstall -> EngineInstallHandler.props(
        runtimeVersionManagementService
      ),
      EngineUninstall -> EngineUninstallHandler.props(
        runtimeVersionManagementService
      ),
      ConfigGet -> ConfigGetHandler
        .props(globalConfigService, timeoutConfig.requestTimeout),
      ConfigSet -> ConfigSetHandler
        .props(globalConfigService, timeoutConfig.requestTimeout),
      ConfigDelete -> ConfigDeleteHandler
        .props(globalConfigService, timeoutConfig.requestTimeout),
      LoggingServiceGetEndpoint -> LoggingServiceEndpointRequestHandler.props(
        loggingServiceDescriptor,
        timeoutConfig.requestTimeout
      )
    )

  override def receive: Receive = {
    case JsonRpcServer.WebConnect(webActor) =>
      logger.info("Client connected to Project Manager [{}]", clientId)
      unstashAll()
      context.become(connected(webActor))
      context.system.eventStream.publish(ClientConnected(clientId))

    case _ => stash()
  }

  def connected(@unused webActor: ActorRef): Receive = {
    case MessageHandler.Disconnected =>
      logger.info("Client disconnected from the Project Manager [{}]", clientId)
      context.system.eventStream.publish(ClientDisconnected(clientId))
      context.stop(self)

    case r @ Request(method, _, _) if requestHandlers.contains(method) =>
      val handler = context.actorOf(
        requestHandlers(method),
        s"request-handler-$method-${UUID.randomUUID()}"
      )
      handler.forward(r)
  }
}

object ClientController {

  /** Creates a configuration object used to create a [[ClientController]].
    *
    * @param clientId the internal client id.
    * @param projectService a project service
    * @param globalConfigService global configuration service
    * @param runtimeVersionManagementService version management service
    * @param timeoutConfig a request timeout config
    * @param loggingServiceDescriptor a logging service configuration descriptor
    * @return a configuration object
    */
  def props[F[+_, +_]: Exec: CovariantFlatMap: ErrorChannel](
    clientId: UUID,
    projectService: ProjectServiceApi[F],
    globalConfigService: GlobalConfigServiceApi[F],
    runtimeVersionManagementService: RuntimeVersionManagementServiceApi[F],
    loggingServiceDescriptor: LoggingServiceDescriptor,
    timeoutConfig: TimeoutConfig
  ): Props =
    Props(
      new ClientController(
        clientId                        = clientId,
        projectService                  = projectService,
        globalConfigService             = globalConfigService,
        runtimeVersionManagementService = runtimeVersionManagementService,
        loggingServiceDescriptor        = loggingServiceDescriptor,
        timeoutConfig                   = timeoutConfig
      )
    )

}
