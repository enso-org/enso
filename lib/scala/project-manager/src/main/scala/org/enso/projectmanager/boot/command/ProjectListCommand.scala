package org.enso.projectmanager.boot.command

import org.enso.projectmanager.boot.configuration.ProjectManagerConfig
import org.enso.projectmanager.control.core.syntax._
import org.enso.projectmanager.control.effect.syntax._
import org.enso.projectmanager.control.core.{Applicative, CovariantFlatMap}
import org.enso.projectmanager.control.effect.{ErrorChannel, Sync}
import org.enso.projectmanager.infrastructure.file.BlockingFileSystem
import org.enso.projectmanager.infrastructure.random.SystemGenerator
import org.enso.projectmanager.infrastructure.repository.{
  ProjectFileRepository,
  ProjectRepository
}
import org.enso.projectmanager.infrastructure.time.RealClock
import org.enso.projectmanager.protocol.ProjectManagementApi.ProjectList
import org.enso.projectmanager.service.{
  ProjectService,
  ProjectServiceFailure,
  RecentlyUsedProjectsOrdering
}

import java.io.File

final class ProjectListCommand[
  F[+_, +_]: ErrorChannel: CovariantFlatMap
](repo: ProjectRepository[F], limitOpt: Option[Int]) {

  def run: F[ProjectServiceFailure, ProjectList.Result] =
    repo
      .getAll()
      .map(
        _.sorted(RecentlyUsedProjectsOrdering)
          .take(limitOpt.getOrElse(Int.MaxValue))
      )
      .mapError(ProjectService.toServiceFailure)
      .map(_.map(ProjectService.toProjectMetadata))
      .map(ProjectList.Result)
}

object ProjectListCommand {

  def apply[F[+_, +_]: Applicative: Sync: ErrorChannel: CovariantFlatMap](
    config: ProjectManagerConfig,
    projectsPath: Option[File],
    limitOpt: Option[Int]
  ): ProjectListCommand[F] = {
    val clock      = new RealClock[F]
    val fileSystem = new BlockingFileSystem[F](config.timeout.ioTimeout)
    val gen        = new SystemGenerator[F]
    val storageConfig = projectsPath.fold(config.storage)(path =>
      config.storage.copy(userProjectsPath = path)
    )

    val projectRepository =
      new ProjectFileRepository[F](
        storageConfig,
        clock,
        fileSystem,
        gen
      )

    new ProjectListCommand[F](projectRepository, limitOpt)
  }
}
