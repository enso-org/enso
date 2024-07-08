package org.enso.projectmanager.infrastructure.repository
import org.enso.projectmanager.boot.configuration.StorageConfig
import org.enso.projectmanager.control.core.{Applicative, CovariantFlatMap}
import org.enso.projectmanager.control.effect.{ErrorChannel, Sync}
import org.enso.projectmanager.infrastructure.file.FileSystem
import org.enso.projectmanager.infrastructure.random.Generator
import org.enso.projectmanager.infrastructure.time.Clock

import java.io.File

class ProjectFileRepositoryFactory[
  F[+_, +_]: Sync: ErrorChannel: CovariantFlatMap: Applicative
](
  storageConfig: StorageConfig,
  clock: Clock[F],
  fileSystem: FileSystem[F],
  gen: Generator[F]
) extends ProjectRepositoryFactory[F] {

  /** @inheritdoc */
  override def getProjectRepository(
    projectsDirectory: Option[File],
    projectSubdirectory: Option[File]
  ): ProjectRepository[F] = {
    val projectsPath =
      projectsDirectory.getOrElse(storageConfig.userProjectsPath).toPath
    new ProjectFileRepository[F](
      projectSubdirectory
        .map(subdir => projectsPath.resolve(subdir.toPath))
        .getOrElse(projectsPath)
        .toFile,
      storageConfig.metadata,
      clock,
      fileSystem,
      gen
    )
  }
}
