package org.enso.projectmanager.service

import java.util.UUID

import org.enso.projectmanager.data.{LanguageServerSockets, ProjectMetadata}

/**
  * A contract for the Project Service.
  *
  * @tparam F a monadic context
  */
trait ProjectServiceApi[F[+_, +_]] {

  /**
    * Creates a user project.
    *
    * @param name the name of th project
    * @return projectId
    */
  def createUserProject(name: String): F[ProjectServiceFailure, UUID]

  /**
    * Deletes a user project.
    *
    * @param projectId the project id
    * @return either failure or unit representing success
    */
  def deleteUserProject(projectId: UUID): F[ProjectServiceFailure, Unit]

  /**
    * Renames a project.
    *
    * @param projectId the project id
    * @param name the new name
    * @return either failure or unit representing success
    */
  def renameProject(
    projectId: UUID,
    name: String
  ): F[ProjectServiceFailure, Unit]

  /**
    * Opens a project. It starts up a Language Server if needed.
    *
    * @param clientId the requester id
    * @param projectId the project id
    * @return either failure or a socket of the Language Server
    */
  def openProject(
    clientId: UUID,
    projectId: UUID
  ): F[ProjectServiceFailure, LanguageServerSockets]

  /**
    * Closes a project. Tries to shut down the Language Server.
    *
    * @param clientId the requester id
    * @param projectId the project id
    * @return either failure or [[Unit]] representing void success
    */
  def closeProject(
    clientId: UUID,
    projectId: UUID
  ): F[ProjectServiceFailure, Unit]

  /**
    * Lists the user's most recently opened projects..
    *
    * @param maybeSize the size of result set
    * @return list of recent projects
    */
  def listProjects(
    maybeSize: Option[Int]
  ): F[ProjectServiceFailure, List[ProjectMetadata]]

}
