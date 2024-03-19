package org.enso.projectmanager.service.filesystem

import org.enso.projectmanager.control.core.{Applicative, CovariantFlatMap, Traverse}
import org.enso.projectmanager.control.effect.ErrorChannel
import org.enso.projectmanager.infrastructure.file.FileSystem
import org.enso.projectmanager.control.core.syntax._
import org.enso.projectmanager.control.effect.syntax._
import org.enso.projectmanager.infrastructure.repository.ProjectRepositoryFactory
import org.enso.projectmanager.service.ProjectService

import java.io.{File, InputStream}
import java.nio.file.Files
import java.nio.file.attribute.BasicFileAttributes

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
  override def delete(path: File): F[FileSystemServiceFailure, Unit] =
    fileSystem
      .remove(path)
      .mapError(_ =>
        FileSystemServiceFailure.FileSystem("Failed to delete path")
      )

  /** @inheritdoc */
  override def move(from: File, to: File): F[FileSystemServiceFailure, Unit] =
    fileSystem
      .move(from, to)
      .mapError(_ => FileSystemServiceFailure.FileSystem("Failed to move path"))

  /** @inheritdoc */
  override def write(
    path: File,
    contents: InputStream
  ): F[FileSystemServiceFailure, Unit] =
    fileSystem
      .writeFile(path, contents)
      .mapError(_ =>
        FileSystemServiceFailure.FileSystem("Failed to write path")
      )

  private def toFileSystemEntry(
    path: File
  ): F[FileSystemServiceFailure, Option[FileSystemEntry]] = {
    val file = normalize(path)
    val projectRepository =
      projectRepositoryFactory.getProjectRepository(Some(file))
    val basicFileAttributes =
      Files.readAttributes(file.toPath, classOf[BasicFileAttributes])
    val attributes = Attributes(basicFileAttributes)
    if (file.isFile)
      CovariantFlatMap[F].pure(
        Some(FileSystemEntry.FileEntry(file, attributes))
      )
    else if (file.isDirectory) {
      projectRepository
        .tryLoadProject(file)
        .map(
          _.fold[FileSystemEntry](
            FileSystemEntry.DirectoryEntry(file, attributes)
          )(project =>
            FileSystemEntry
              .ProjectEntry(
                file,
                attributes,
                ProjectService.toProjectMetadata(project)
              )
          )
        )
        .map(Some(_))
        .mapError(e =>
          FileSystemServiceFailure
            .ProjectRepository("Failed to load the project", e.message)
        )
    } else CovariantFlatMap[F].pure(None)
  }

  private def normalize(file: File): File =
    file.toPath.toAbsolutePath.normalize().toFile
}
