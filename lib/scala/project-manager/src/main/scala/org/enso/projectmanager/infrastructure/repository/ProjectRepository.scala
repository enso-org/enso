package org.enso.projectmanager.infrastructure.repository

import java.io.File
import java.nio.file.Path
import java.util.UUID

import org.enso.projectmanager.model.Project

/** An abstraction for accessing project domain objects from durable storage.
  *
  * @tparam F a monadic context
  */
trait ProjectRepository[F[+_, +_]] {

  /** Tests if project is present in the data storage.
    *
    * @param name a project name
    * @return true if project exists
    */
  def exists(name: String): F[ProjectRepositoryFailure, Boolean]

  /** Ensures that the path property is set in the project.
    *
    * If it was not set, a new path is generated for it. Otherwise, the function
    * acts as identity.
    *
    * @param moduleName the module name to find the path for
    * @return the project, with the updated path
    */
  def findPathForNewProject(
    moduleName: String
  ): F[ProjectRepositoryFailure, Path]

  /** Saves the provided user project in the index.
    *
    * @param project the project to update
    * @return
    */
  def update(project: Project): F[ProjectRepositoryFailure, Unit]

  /** Removes the provided project from the storage.
    *
    * @param projectId the project id to remove
    * @return either failure or success
    */
  def delete(projectId: UUID): F[ProjectRepositoryFailure, Unit]

  /** Renames a project.
    *
    * @param projectId the project id to rename
    * @param name the new name
    * @return either failure or success
    */
  def rename(projectId: UUID, name: String): F[ProjectRepositoryFailure, Unit]

  /** Finds a project by project id.
    *
    * @param projectId a project id
    * @return option with the project entity
    */
  def findById(
    projectId: UUID
  ): F[ProjectRepositoryFailure, Option[Project]]

  /** Finds projects that meet criteria specified by predicate.
    *
    * @param predicate a predicate function
    * @return projects that meet the criteria
    */
  def find(
    predicate: Project => Boolean
  ): F[ProjectRepositoryFailure, List[Project]]

  /** Gets all projects from the data store.
    *
    * @return all projects stored in the project index
    */
  def getAll(): F[ProjectRepositoryFailure, List[Project]]

  /** Moves project to the target dir.
    *
    * @param projectId the project id
    * @param newName the new project name
    */
  def moveProjectToTargetDir(
    projectId: UUID,
    newName: String
  ): F[ProjectRepositoryFailure, File]

  /** Gets a package name for the specified project.
    *
    * @param projectId the project id
    * @return either a failure or a package name
    */
  def getPackageName(projectId: UUID): F[ProjectRepositoryFailure, String]

  /** Gets a package namespace for the specified project.
    *
    * @param projectId the project id
    * @return either a failure or a package namespace
    */
  def getPackageNamespace(projectId: UUID): F[ProjectRepositoryFailure, String]
}
