package org.enso.projectmanager.infrastructure.repository

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
    * Inserts the provided user project to the storage.
    *
    * @param project the project to insert
    * @return
    */
  def insertUserProject(
    project: Project
  ): F[ProjectRepositoryFailure, Unit]

  /**
    * Removes the provided project from the storage.
    *
    * @param projectId the project id to remove
    * @return either failure or success
    */
  def deleteUserProject(projectId: UUID): F[ProjectRepositoryFailure, Unit]

}
