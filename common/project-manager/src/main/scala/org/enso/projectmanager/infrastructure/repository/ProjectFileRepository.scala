package org.enso.projectmanager.infrastructure.repository

import java.io.File
import java.util.UUID

import org.enso.pkg.Package
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
import org.enso.projectmanager.boot.configuration.StorageConfig
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

  /**
    * Tests if project is present in the data storage.
    *
    * @param name a project name
    * @return true if project exists
    */
  override def exists(
    name: String
  ): F[ProjectRepositoryFailure, Boolean] =
    indexStorage
      .load()
      .map(_.exists(name))
      .mapError(_.fold(convertFileStorageFailure))

  /** @inheritdoc **/
  override def findUserProject(
    projectId: UUID
  ): F[ProjectRepositoryFailure, Option[Project]] =
    indexStorage
      .load()
      .map(_.userProjects.get(projectId))
      .mapError(_.fold(convertFileStorageFailure))

  /**
    * Inserts the provided user project to the storage.
    *
    * @param project the project to insert
    * @return
    */
  override def insertUserProject(
    project: Project
  ): F[ProjectRepositoryFailure, Unit] = {
    val projectPath     = new File(storageConfig.userProjectsPath, project.name)
    val projectWithPath = project.copy(path = Some(projectPath.toString))

    createProjectStructure(project, projectPath) *>
    indexStorage
      .modify { index =>
        val updated = index.addUserProject(projectWithPath)
        (updated, ())
      }
      .mapError(_.fold(convertFileStorageFailure))
  }

  private def createProjectStructure(
    project: Project,
    projectPath: File
  ): F[StorageFailure, Package] =
    Sync[F]
      .blockingOp { Package.create(projectPath, project.name) }
      .mapError(th => StorageFailure(th.toString))

  /**
    * Removes the provided project from the storage.
    *
    * @param projectId the project id to remove
    * @return either failure or success
    */
  override def deleteUserProject(
    projectId: UUID
  ): F[ProjectRepositoryFailure, Unit] =
    indexStorage
      .modify { index =>
        val maybeProject = index.findUserProject(projectId)
        index.removeUserProject(projectId) -> maybeProject
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
      .mapError[ProjectRepositoryFailure](
        failure => StorageFailure(failure.toString)
      )

}
