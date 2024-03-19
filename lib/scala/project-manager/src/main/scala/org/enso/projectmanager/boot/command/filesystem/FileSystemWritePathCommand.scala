package org.enso.projectmanager.boot.command.filesystem

import org.apache.commons.io.IOUtils
import org.enso.projectmanager.boot.configuration.ProjectManagerConfig
import org.enso.projectmanager.control.core.syntax._
import org.enso.projectmanager.control.core.{Applicative, CovariantFlatMap}
import org.enso.projectmanager.control.effect.{ErrorChannel, Sync}
import org.enso.projectmanager.infrastructure.file.BlockingFileSystem
import org.enso.projectmanager.infrastructure.random.SystemGenerator
import org.enso.projectmanager.infrastructure.repository.ProjectFileRepositoryFactory
import org.enso.projectmanager.infrastructure.time.RealClock
import org.enso.projectmanager.protocol.FileSystemManagementApi.FileSystemMoveDirectory
import org.enso.projectmanager.service.filesystem.{FileSystemService, FileSystemServiceApi, FileSystemServiceFailure}

import java.io.File

final class FileSystemWritePathCommand[F[+_, +_]: CovariantFlatMap](
  service: FileSystemServiceApi[F],
  path: File,
  data: Array[Byte]
) {

  def run: F[FileSystemServiceFailure, FileSystemMoveDirectory.Result] =
    service.write(path, data).map(_ => FileSystemMoveDirectory.Result)
}

object FileSystemWritePathCommand {

  def apply[F[+_, +_]: Applicative: CovariantFlatMap: ErrorChannel: Sync](
    config: ProjectManagerConfig,
    path: File,
  ): FileSystemWritePathCommand[F] = {
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

    val data = IOUtils.toByteArray(System.in)

    new FileSystemWritePathCommand[F](service, path, data)
  }
}
