package org.enso.projectmanager.boot.command.filesystem

import org.enso.projectmanager.boot.configuration.ProjectManagerConfig
import org.enso.projectmanager.control.core.syntax._
import org.enso.projectmanager.control.core.{Applicative, CovariantFlatMap}
import org.enso.projectmanager.control.effect.{ErrorChannel, Sync}
import org.enso.projectmanager.infrastructure.file.BlockingFileSystem
import org.enso.projectmanager.infrastructure.random.SystemGenerator
import org.enso.projectmanager.infrastructure.repository.ProjectFileRepositoryFactory
import org.enso.projectmanager.infrastructure.time.RealClock
import org.enso.projectmanager.protocol.FileSystemManagementApi.FileSystemMoveDirectory
import org.enso.projectmanager.service.filesystem.{
  FileSystemService,
  FileSystemServiceApi,
  FileSystemServiceFailure
}

import java.io.File

final class FileSystemMoveDirectoryCommand[F[+_, +_]: CovariantFlatMap](
  service: FileSystemServiceApi[F],
  from: File,
  to: File
) {

  def run: F[FileSystemServiceFailure, FileSystemMoveDirectory.Result] =
    service.move(from, to).map(_ => FileSystemMoveDirectory.Result)
}

object FileSystemMoveDirectoryCommand {

  def apply[F[+_, +_]: Applicative: CovariantFlatMap: ErrorChannel: Sync](
    config: ProjectManagerConfig,
    from: File,
    to: File
  ): FileSystemMoveDirectoryCommand[F] = {
    val clock      = new RealClock[F]
    val fileSystem = new BlockingFileSystem[F](config.timeout.ioTimeout)
    val gen        = new SystemGenerator[F]
    val projectRepositoryFactory = new ProjectFileRepositoryFactory[F](
      config.storage,
      clock,
      fileSystem,
      gen
    )

    val service = new FileSystemService[F](fileSystem, projectRepositoryFactory)

    new FileSystemMoveDirectoryCommand[F](service, from, to)
  }
}
