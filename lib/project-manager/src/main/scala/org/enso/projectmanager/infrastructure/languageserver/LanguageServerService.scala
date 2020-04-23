package org.enso.projectmanager.infrastructure.languageserver

import java.util.UUID

import org.enso.projectmanager.data.LanguageServerSockets
import org.enso.projectmanager.infrastructure.languageserver.LanguageServerProtocol.{
  CheckTimeout,
  ServerStartupFailure,
  ServerStoppageFailure
}
import org.enso.projectmanager.model.Project

/**
  * A infrastructure service for managing lang. servers.
  *
  * @tparam F a effectful context
  */
trait LanguageServerService[F[+_, +_]] {

  /**
    * Starts a language server.
    *
    * @param clientId a requester id
    * @param project a project to start
    * @return either a failure or sockets that a language server listens on
    */
  def start(
    clientId: UUID,
    project: Project
  ): F[ServerStartupFailure, LanguageServerSockets]

  /**
    * Stops a lang. server.
    *
    * @param clientId a requester id
    * @param projectId a project id to stop
    * @return either failure or Unit representing void success
    */
  def stop(
    clientId: UUID,
    projectId: UUID
  ): F[ServerStoppageFailure, Unit]

  /**
    * Checks if server is running for project.
    *
    * @param projectId a project id
    * @return true if project is open
    */
  def isRunning(projectId: UUID): F[CheckTimeout.type, Boolean]

}
