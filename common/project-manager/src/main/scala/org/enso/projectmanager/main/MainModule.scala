package org.enso.projectmanager.main

import akka.actor.ActorSystem
import akka.stream.SystemMaterializer
import cats.{Bifunctor, MonadError}
import io.circe.generic.auto._
import org.enso.jsonrpc.JsonRpcServer
import org.enso.projectmanager.control.core.CovariantFlatMap
import org.enso.projectmanager.control.effect.{ErrorChannel, Exec, Sync}
import org.enso.projectmanager.infrastructure.file.{
  BlockingFileSystem,
  SynchronizedFileStorage
}
import org.enso.projectmanager.infrastructure.log.Slf4jLogging
import org.enso.projectmanager.infrastructure.random.SystemGenerator
import org.enso.projectmanager.infrastructure.repository.{
  ProjectFileRepository,
  ProjectIndex
}
import org.enso.projectmanager.infrastructure.time.RealClock
import org.enso.projectmanager.main.configuration.ProjectManagerConfig
import org.enso.projectmanager.protocol.{
  JsonRpc,
  ManagerClientControllerFactory
}
import org.enso.projectmanager.service.{
  MonadicProjectValidator,
  ProjectService,
  ProjectServiceFailure,
  ValidationFailure
}

/**
  * A main module containing all components of the project manager.
  *
  */
class MainModule[F[+_, +_]: Sync: ErrorChannel: Exec: CovariantFlatMap: Bifunctor](
  config: ProjectManagerConfig
)(
  implicit E1: MonadError[F[ProjectServiceFailure, *], ProjectServiceFailure],
  E2: MonadError[F[ValidationFailure, *], ValidationFailure]
) {

  implicit val system = ActorSystem()

  implicit val materializer = SystemMaterializer.get(system)

  lazy val logging = new Slf4jLogging[F]

  lazy val clock = new RealClock[F]

  lazy val fileSystem =
    new BlockingFileSystem[F](config.timeout.ioTimeout)

  lazy val indexStorage = new SynchronizedFileStorage[ProjectIndex, F](
    config.storage.projectMetadataPath,
    fileSystem
  )

  lazy val projectRepository =
    new ProjectFileRepository[F](
      config.storage,
      fileSystem,
      indexStorage
    )

  lazy val gen = new SystemGenerator[F]

  lazy val projectValidator = new MonadicProjectValidator[F]()

  lazy val projectService =
    new ProjectService[F](
      projectValidator,
      projectRepository,
      logging,
      clock,
      gen
    )

  lazy val clientControllerFactory =
    new ManagerClientControllerFactory[F](
      system,
      projectService,
      config.timeout.requestTimeout
    )

  lazy val server = new JsonRpcServer(JsonRpc.protocol, clientControllerFactory)

}
