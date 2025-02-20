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
import org.slf4j.LoggerFactory

import java.io.{File, InputStream, OutputStream}
import java.nio.file.Files
import java.nio.file.attribute.BasicFileAttributes

class FileSystemService[F[+_, +_]: Applicative: CovariantFlatMap: ErrorChannel](
  fileSystem: FileSystem[F],
  projectRepositoryFactory: ProjectRepositoryFactory[F]
) extends FileSystemServiceApi[F] {

  private lazy val logger = LoggerFactory.getLogger(this.getClass)

  /** @inheritdoc */
  override def exists(path: File): F[FileSystemServiceFailure, Boolean] =
    fileSystem
      .exists(path)
      .mapError { error =>
        logger.warn("Failed to check if path exists", error)
        FileSystemServiceFailure.FileSystem("Failed to check if path exists")
      }

  /** @inheritdoc */
  override def list(
    path: File
  ): F[FileSystemServiceFailure, Seq[FileSystemEntry]] =
    fileSystem
      .list(path)
      .mapError { error =>
        logger.warn("Failed to list directories", error)
        FileSystemServiceFailure.FileSystem("Failed to list directories")
      }
      .flatMap { files =>
        Traverse[List].traverse(files)(toFileSystemEntry).map(_.flatten)
      }

  /** @inheritdoc */
  override def createDirectory(path: File): F[FileSystemServiceFailure, Unit] =
    fileSystem
      .createDir(path)
      .mapError { error =>
        logger.warn("Failed to create directory", error)
        FileSystemServiceFailure.FileSystem("Failed to create directory")
      }

  /** @inheritdoc */
  override def delete(path: File): F[FileSystemServiceFailure, Unit] =
    fileSystem
      .remove(path)
      .mapError { error =>
        logger.warn("Failed to delete path", error)
        FileSystemServiceFailure.FileSystem("Failed to delete path")
      }

  /** @inheritdoc */
  override def move(from: File, to: File): F[FileSystemServiceFailure, Unit] =
    fileSystem
      .move(from, to)
      .mapError { error =>
        logger.warn("Failed to list directories", error)
        FileSystemServiceFailure.FileSystem("Failed to move path")
      }

  /** @inheritdoc */
  override def copy(from: File, to: File): F[FileSystemServiceFailure, Unit] =
    fileSystem
      .copy(from, to)
      .mapError { error =>
        logger.warn("Failed to copy path", error)
        FileSystemServiceFailure.FileSystem("Failed to copy path")
      }

  /** @inheritdoc */
  override def read(
    path: File,
    output: OutputStream
  ): F[FileSystemServiceFailure, Int] =
    fileSystem
      .readFile(path, output)
      .mapError { error =>
        logger.warn("Failed to read path", error)
        FileSystemServiceFailure.FileSystem("Failed to read path")
      }

  /** @inheritdoc */
  override def write(
    path: File,
    contents: InputStream
  ): F[FileSystemServiceFailure, Unit] =
    fileSystem
      .writeFile(path, contents)
      .mapError { error =>
        logger.warn("Failed to write path", error)
        FileSystemServiceFailure.FileSystem("Failed to write path")
      }

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
