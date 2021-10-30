package org.enso.projectmanager.boot

import akka.actor.ActorSystem
import akka.stream.SystemMaterializer
import cats.MonadError
import org.enso.jsonrpc.JsonRpcServer
import org.enso.loggingservice.LogLevel
import org.enso.projectmanager.boot.configuration.ProjectManagerConfig
import org.enso.projectmanager.control.core.{Applicative, CovariantFlatMap}
import org.enso.projectmanager.control.effect.{Async, ErrorChannel, Exec, Sync}
import org.enso.projectmanager.infrastructure.file.BlockingFileSystem
import org.enso.projectmanager.infrastructure.languageserver.{
  ExecutorWithUnlimitedPool,
  LanguageServerGatewayImpl,
  LanguageServerRegistry,
  ShutdownHookActivator
}
import org.enso.projectmanager.infrastructure.log.Slf4jLogging
import org.enso.projectmanager.infrastructure.random.SystemGenerator
import org.enso.projectmanager.infrastructure.repository.ProjectFileRepository
import org.enso.projectmanager.infrastructure.time.RealClock
import org.enso.projectmanager.protocol.{
  JsonRpc,
  ManagerClientControllerFactory
}
import org.enso.projectmanager.service.config.GlobalConfigService
import org.enso.projectmanager.service.versionmanagement.RuntimeVersionManagementService
import org.enso.projectmanager.service._
import org.enso.projectmanager.versionmanagement.DefaultDistributionConfiguration

import scala.concurrent.ExecutionContext

/** A main module containing all components of the project manager.
  */
class MainModule[
  F[+_, +_]: Sync: ErrorChannel: Exec: CovariantFlatMap: Applicative: Async
](
  config: ProjectManagerConfig,
  logLevel: LogLevel,
  computeExecutionContext: ExecutionContext
)(implicit
  E1: MonadError[F[ProjectServiceFailure, *], ProjectServiceFailure],
  E2: MonadError[F[ValidationFailure, *], ValidationFailure]
) {

  implicit val system =
    ActorSystem("project-manager", None, None, Some(computeExecutionContext))
  system.eventStream.setLogLevel(LogLevel.toAkka(logLevel))

  implicit val materializer = SystemMaterializer.get(system)

  lazy val logging = new Slf4jLogging[F]

  lazy val clock = new RealClock[F]

  lazy val fileSystem =
    new BlockingFileSystem[F](config.timeout.ioTimeout)

  lazy val gen = new SystemGenerator[F]

  lazy val projectValidator = new MonadicProjectValidator[F]()

  lazy val projectRepository =
    new ProjectFileRepository[F](
      config.storage,
      clock,
      fileSystem,
      gen
    )

  val distributionConfiguration = DefaultDistributionConfiguration
  val loggingService            = Logging.GlobalLoggingService

  lazy val languageServerRegistry =
    system.actorOf(
      LanguageServerRegistry
        .props(
          config.network,
          config.bootloader,
          config.supervision,
          config.timeout,
          distributionConfiguration,
          loggingService,
          ExecutorWithUnlimitedPool
        ),
      "language-server-registry"
    )

  lazy val shutdownHookActivator =
    system.actorOf(
      ShutdownHookActivator.props[F](),
      "language-server-shutdown-hook-activator"
    )

  lazy val languageServerGateway = new LanguageServerGatewayImpl[F](
    languageServerRegistry,
    shutdownHookActivator,
    system,
    config.timeout
  )

  lazy val projectCreationService =
    new ProjectCreationService[F](
      distributionConfiguration,
      loggingService
    )

  lazy val globalConfigService =
    new GlobalConfigService[F](distributionConfiguration)

  lazy val projectService =
    new ProjectService[F](
      projectValidator,
      projectRepository,
      projectCreationService,
      globalConfigService,
      logging,
      clock,
      gen,
      languageServerGateway,
      distributionConfiguration
    )

  lazy val runtimeVersionManagementService =
    new RuntimeVersionManagementService[F](distributionConfiguration)

  lazy val clientControllerFactory =
    new ManagerClientControllerFactory[F](
      system                          = system,
      projectService                  = projectService,
      globalConfigService             = globalConfigService,
      runtimeVersionManagementService = runtimeVersionManagementService,
      loggingServiceDescriptor        = loggingService,
      timeoutConfig                   = config.timeout
    )

  lazy val server = new JsonRpcServer(JsonRpc.protocol, clientControllerFactory)
}
