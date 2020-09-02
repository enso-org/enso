package org.enso.projectmanager.infrastructure.repository

import java.io.File
import java.time.OffsetDateTime
import java.util.UUID

import io.circe.generic.auto._
import org.enso.projectmanager.control.core.CovariantFlatMap
import org.enso.projectmanager.control.core.syntax._
import org.enso.projectmanager.control.effect.syntax._
import org.enso.projectmanager.control.effect.{ErrorChannel, Sync}
import org.enso.projectmanager.infrastructure.file.{
  FileSystem,
  SynchronizedFileStorage
}
import org.enso.projectmanager.infrastructure.repository.ProjectRepositoryFailure.StorageFailure
import org.enso.projectmanager.infrastructure.time.Clock
import org.enso.projectmanager.model.{ProjectKind, ProjectMetadata}

final class ProjectMetadataStorage[
  F[+_, +_]: Sync: ErrorChannel: CovariantFlatMap
](
  directory: File,
  clock: Clock[F],
  fileSystem: FileSystem[F]
) {

  private val file = new SynchronizedFileStorage[ProjectMetadata, F](
    ProjectMetadataStorage.metadataPath(directory),
    fileSystem
  )

  def load: F[ProjectRepositoryFailure, ProjectMetadata] =
    file
      .load()
      .mapError(_.fold(convertFileStorageFailure))
      .recoverWith(_ => init)

  def init: F[ProjectRepositoryFailure, ProjectMetadata] =
    for {
      now <- clock.nowInUtc()
      metadata = initialProjectMetadata(now)
      _ <-
        file
          .persist(metadata)
          .mapError(f => StorageFailure(f.toString))
    } yield metadata

  def persist(metadata: ProjectMetadata): F[ProjectRepositoryFailure, Unit] =
    file.persist(metadata).mapError(th => StorageFailure(th.toString))

  def modify[A](
    f: ProjectMetadata => (ProjectMetadata, A)
  ): F[ProjectRepositoryFailure, A] =
    file.modify(f).mapError(_.fold(convertFileStorageFailure))

  private def initialProjectMetadata(
    creationTime: OffsetDateTime
  ): ProjectMetadata = {
    ProjectMetadata(
      id         = UUID.randomUUID(),
      kind       = ProjectKind.UserProject,
      created    = creationTime,
      lastOpened = None
    )
  }

}

object ProjectMetadataStorage {

  val MetadataFile = "project.json"

  def metadataPath(project: File): File =
    new File(project, new File(".enso", MetadataFile).toString)

}
