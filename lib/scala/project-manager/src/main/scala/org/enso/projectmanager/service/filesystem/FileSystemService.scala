package org.enso.projectmanager.service.filesystem

import org.enso.projectmanager.control.core.{
  Applicative,
  CovariantFlatMap,
  Traverse
}
import org.enso.projectmanager.control.effect.ErrorChannel
import org.enso.projectmanager.infrastructure.file.FileSystem
import org.enso.projectmanager.control.core.syntax._
import org.enso.projectmanager.control.effect.syntax._
import org.enso.projectmanager.infrastructure.repository.ProjectRepositoryFactory
import org.enso.projectmanager.service.ProjectService

import java.io.File

class FileSystemService[F[+_, +_]: Applicative: CovariantFlatMap: ErrorChannel](
  fileSystem: FileSystem[F],
  projectRepositoryFactory: ProjectRepositoryFactory[F]
) extends FileSystemServiceApi[F] {

  /** @inheritdoc */
  override def list(
    path: File
  ): F[FileSystemServiceFailure, Seq[FileSystemEntry]] =
    fileSystem
      .list(path)
      .mapError(_ =>
        FileSystemServiceFailure.FileSystem("Failed to list directories")
      )
      .flatMap { files =>
        Traverse[List].traverse(files)(toFileSystemEntry).map(_.flatten)
      }

  /** @inheritdoc */
  override def createDirectory(path: File): F[FileSystemServiceFailure, Unit] =
    fileSystem
      .createDir(path)
      .mapError(_ =>
        FileSystemServiceFailure.FileSystem("Failed to create directory")
      )

  /** @inheritdoc */
  override def deleteDirectory(path: File): F[FileSystemServiceFailure, Unit] =
    fileSystem
      .removeDir(path)
      .mapError(_ =>
        FileSystemServiceFailure.FileSystem("Failed to delete directory")
      )

  private def toFileSystemEntry(
    file: File
  ): F[FileSystemServiceFailure, Option[FileSystemEntry]] = {
    val projectRepository =
      projectRepositoryFactory.getProjectRepository(Some(file))
    if (file.isFile)
      CovariantFlatMap[F].pure(Some(FileSystemEntry.FileEntry(file)))
    else if (file.isDirectory) {
      projectRepository
        .tryLoadProject(file)
        .map(
          _.fold[FileSystemEntry](FileSystemEntry.DirectoryEntry(file))(
            project =>
              FileSystemEntry
                .ProjectEntry(file, ProjectService.toProjectMetadata(project))
          )
        )
        .map(Some(_))
        .mapError(e =>
          FileSystemServiceFailure
            .ProjectRepository("Failed to load the project", e.message)
        )
    } else CovariantFlatMap[F].pure(None)
  }
}
