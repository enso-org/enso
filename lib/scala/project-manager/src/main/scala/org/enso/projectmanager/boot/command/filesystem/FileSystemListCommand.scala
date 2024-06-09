package org.enso.projectmanager.boot.command.filesystem

import org.enso.projectmanager.boot.configuration.ProjectManagerConfig
import org.enso.projectmanager.control.core.syntax._
import org.enso.projectmanager.control.core.{Applicative, CovariantFlatMap}
import org.enso.projectmanager.control.effect.{ErrorChannel, Sync}
import org.enso.projectmanager.infrastructure.file.BlockingFileSystem
import org.enso.projectmanager.infrastructure.random.SystemGenerator
import org.enso.projectmanager.infrastructure.repository.ProjectFileRepositoryFactory
import org.enso.projectmanager.infrastructure.time.RealClock
import org.enso.projectmanager.protocol.FileSystemManagementApi.FileSystemList
import org.enso.projectmanager.service.filesystem.{
  FileSystemEntry,
  FileSystemService,
  FileSystemServiceApi,
  FileSystemServiceFailure
}

import java.io.File

final class FileSystemListCommand[
  F[+_, +_]: CovariantFlatMap
](service: FileSystemServiceApi[F], path: File) {

  def run: F[FileSystemServiceFailure, FileSystemList.Result] =
    service
      .list(path)
      .map(FileSystemListCommand.filterNotHidden)
      .map(FileSystemList.Result)
}

object FileSystemListCommand {

  def apply[F[+_, +_]: Applicative: CovariantFlatMap: ErrorChannel: Sync](
    config: ProjectManagerConfig,
    path: File
  ): FileSystemListCommand[F] = {
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

    new FileSystemListCommand[F](service, path)
  }

  /** Filters the files system entries that are not hidden.
    *
    * @param entries the file system entries
    * @return the filtered list of entries
    */
  private def filterNotHidden(
    entries: Seq[FileSystemEntry]
  ): Seq[FileSystemEntry] =
    entries.filterNot(_.path.isHidden)

}
