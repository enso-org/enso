package org.enso.projectmanager.infrastructure.repository

import java.io.File
import java.util.UUID

import org.enso.pkg.{Package, PackageManager}
import org.enso.projectmanager.boot.configuration.StorageConfig
import org.enso.projectmanager.control.core.CovariantFlatMap
import org.enso.projectmanager.control.core.syntax._
import org.enso.projectmanager.control.effect.syntax._
import org.enso.projectmanager.control.effect.{ErrorChannel, Sync}
import org.enso.projectmanager.infrastructure.file.{FileStorage, FileSystem}
import org.enso.projectmanager.infrastructure.repository.ProjectRepositoryFailure.{
  InconsistentStorage,
  ProjectNotFoundInIndex,
  StorageFailure
}
import org.enso.projectmanager.model.Project

/**
  * File based implementation of the project repository.
  *
  * @param storageConfig a storage config
  * @param fileSystem a file system abstraction
  * @param indexStorage an index storage
  */
class ProjectFileRepository[F[+_, +_]: Sync: ErrorChannel: CovariantFlatMap](
  storageConfig: StorageConfig,
  fileSystem: FileSystem[F],
  indexStorage: FileStorage[ProjectIndex, F]
) extends ProjectRepository[F] {

  /** @inheritdoc **/
  override def exists(
    name: String
  ): F[ProjectRepositoryFailure, Boolean] =
    indexStorage
      .load()
      .map(_.exists(name))
      .mapError(_.fold(convertFileStorageFailure))

  /** @inheritdoc **/
  override def find(
    predicate: Project => Boolean
  ): F[ProjectRepositoryFailure, List[Project]] =
    indexStorage
      .load()
      .map(_.find(predicate))
      .mapError(_.fold(convertFileStorageFailure))

  /** @inheritdoc **/
  override def getAll(): F[ProjectRepositoryFailure, List[Project]] =
    indexStorage
      .load()
      .map(_.projects.values.toList)
      .mapError(_.fold(convertFileStorageFailure))

  /** @inheritdoc **/
  override def findById(
    projectId: UUID
  ): F[ProjectRepositoryFailure, Option[Project]] =
    indexStorage
      .load()
      .map(_.findById(projectId))
      .mapError(_.fold(convertFileStorageFailure))

  /** @inheritdoc **/
  override def save(
    project: Project
  ): F[ProjectRepositoryFailure, Unit] = {
    val projectPath     = new File(storageConfig.userProjectsPath, project.name)
    val projectWithPath = project.copy(path = Some(projectPath.toString))

    createProjectStructure(project, projectPath) *>
    indexStorage
      .modify { index =>
        val updated = index.add(projectWithPath)
        (updated, ())
      }
      .mapError(_.fold(convertFileStorageFailure))
  }

  private def createProjectStructure(
    project: Project,
    projectPath: File
  ): F[StorageFailure, Package[File]] =
    Sync[F]
      .blockingOp { PackageManager.Default.create(projectPath, project.name) }
      .mapError(th => StorageFailure(th.toString))

  /** @inheritdoc **/
  override def delete(
    projectId: UUID
  ): F[ProjectRepositoryFailure, Unit] =
    indexStorage
      .modify { index =>
        val maybeProject = index.findById(projectId)
        index.remove(projectId) -> maybeProject
      }
      .mapError(_.fold(convertFileStorageFailure))
      .flatMap {
        case None =>
          ErrorChannel[F].fail(ProjectNotFoundInIndex)

        case Some(project) if project.path.isEmpty =>
          ErrorChannel[F].fail(
            InconsistentStorage(
              "Index cannot contain a user project without path"
            )
          )

        case Some(project) =>
          removeProjectStructure(project.path.get)
      }

  private def removeProjectStructure(
    projectPath: String
  ): F[ProjectRepositoryFailure, Unit] =
    fileSystem
      .removeDir(new File(projectPath))
      .mapError[ProjectRepositoryFailure](failure =>
        StorageFailure(failure.toString)
      )

}
