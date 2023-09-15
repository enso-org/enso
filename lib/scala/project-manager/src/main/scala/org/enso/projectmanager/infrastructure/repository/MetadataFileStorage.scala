package org.enso.projectmanager.infrastructure.repository

import java.io.File

import io.circe.generic.auto._
import org.enso.projectmanager.boot.configuration.StorageConfig
import org.enso.projectmanager.control.core.CovariantFlatMap
import org.enso.projectmanager.control.core.syntax._
import org.enso.projectmanager.control.effect.syntax._
import org.enso.projectmanager.control.effect.ErrorChannel
import org.enso.projectmanager.infrastructure.file.FileStorage.{
  CannotDecodeData,
  LoadFailure
}
import org.enso.projectmanager.infrastructure.file.FileSystemFailure.FileNotFound
import org.enso.projectmanager.infrastructure.file.{
  FileStorage,
  FileSystem,
  FileSystemFailure,
  JsonFileStorage
}
import org.enso.projectmanager.infrastructure.random.Generator
import org.enso.projectmanager.infrastructure.time.Clock
import org.enso.projectmanager.model.{ProjectKind, ProjectMetadata}
import shapeless.{Coproduct, Inl, Inr}

/** File based implementation of the project metadata storage.
  *
  * @param directory a project directory
  * @param storageConfig a storage config
  * @param clock a clock
  * @param fileSystem a file system abstraction
  * @param gen a random generator
  */
final class MetadataFileStorage[
  F[+_, +_]: ErrorChannel: CovariantFlatMap
](
  directory: File,
  storageConfig: StorageConfig,
  clock: Clock[F],
  fileSystem: FileSystem[F],
  gen: Generator[F]
) extends FileStorage[ProjectMetadata, F] {

  private val file = new JsonFileStorage[ProjectMetadata, F](
    metadataPath(directory),
    fileSystem
  )

  /** @inheritdoc */
  override def load(): F[LoadFailure, ProjectMetadata] =
    file
      .load()
      .recoverWith {
        case Inl(CannotDecodeData(_)) =>
          init.mapError(Coproduct[LoadFailure](_))
        case Inr(Inl(FileNotFound)) =>
          init.mapError(Coproduct[LoadFailure](_))
      }

  /** @inheritdoc */
  override def persist(metadata: ProjectMetadata): F[FileSystemFailure, Unit] =
    file.persist(metadata)

  /** @inheritdoc */
  override def modify[A](
    f: ProjectMetadata => (ProjectMetadata, A)
  ): F[LoadFailure, A] =
    file.modify(f)

  private def init: F[FileSystemFailure, ProjectMetadata] =
    for {
      metadata <- freshMetadata
      _        <- file.persist(metadata)
    } yield metadata

  private def freshMetadata: F[FileSystemFailure, ProjectMetadata] =
    for {
      now       <- clock.nowInUtc()
      projectId <- gen.randomUUID()
    } yield ProjectMetadata(
      id         = projectId,
      kind       = ProjectKind.UserProject,
      created    = now,
      lastOpened = None
    )

  private def metadataPath(project: File): File =
    new File(
      project,
      new File(
        storageConfig.projectMetadataDirectory,
        storageConfig.projectMetadataFileName
      ).toString
    )
}
