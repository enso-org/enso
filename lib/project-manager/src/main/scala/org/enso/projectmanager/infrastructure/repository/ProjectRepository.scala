package org.enso.projectmanager.infrastructure.repository

import java.io.File
import java.util.UUID

import org.enso.projectmanager.model.Project

/**
  * An abstraction for accessing project domain objects from durable storage.
  *
  * @tparam F a monadic context
  */
trait ProjectRepository[F[+_, +_]] {

  /**
    * Tests if project is present in the data storage.
    *
    * @param name a project name
    * @return true if project exists
    */
  def exists(name: String): F[ProjectRepositoryFailure, Boolean]

  /**
    * Saves the provided user project in the storage.
    *
    * @param project the project to insert
    * @return
    */
  def create(
    project: Project
  ): F[ProjectRepositoryFailure, Unit]

  /**
    * Removes the provided project from the storage.
    *
    * @param projectId the project id to remove
    * @return either failure or success
    */
  def delete(projectId: UUID): F[ProjectRepositoryFailure, Unit]

  /**
    * Renames a project.
    *
    * @param projectId the project id to rename
    * @param name the new name
    * @return either failure or success
    */
  def rename(projectId: UUID, name: String): F[ProjectRepositoryFailure, Unit]

  /**
    * Finds a project by project id.
    *
    * @param projectId a project id
    * @return option with the project entity
    */
  def findById(
    projectId: UUID
  ): F[ProjectRepositoryFailure, Option[Project]]

  /**
    * Finds projects that meet criteria specified by predicate.
    *
    * @param predicate a predicate function
    * @return projects that meet the criteria
    */
  def find(
    predicate: Project => Boolean
  ): F[ProjectRepositoryFailure, List[Project]]

  /**
    * Gets all projects from the data store.
    *
    * @return all projects stored in the project index
    */
  def getAll(): F[ProjectRepositoryFailure, List[Project]]

  /**
    * Moves project to the target dir.
    *
    * @param projectId the project id
    */
  def moveProjectToTargetDir(projectId: UUID): F[ProjectRepositoryFailure, File]

}
