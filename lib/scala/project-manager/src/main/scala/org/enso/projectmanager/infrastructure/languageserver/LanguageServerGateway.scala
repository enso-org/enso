package org.enso.projectmanager.infrastructure.languageserver

import java.util.UUID

import akka.actor.ActorRef
import org.enso.semver.SemVer
import org.enso.projectmanager.data.LanguageServerSockets
import org.enso.projectmanager.infrastructure.languageserver.LanguageServerProtocol.{
  CheckTimeout,
  ProjectRenameFailure,
  ServerShutdownFailure,
  ServerStartupFailure
}
import org.enso.projectmanager.infrastructure.shutdown.ShutdownHook
import org.enso.projectmanager.model.Project

/** A gateway to lang. server subsystem.
  *
  * @tparam F a effectful context
  */
trait LanguageServerGateway[F[+_, +_]] {

  /** Starts a language server.
    *
    * It assumes that the required engine version has been preinstalled.
    *
    * @param progressTracker an ActorRef that should get notifications when
    *                        waiting on a lock
    * @param clientId a requester id
    * @param project a project to start
    * @param version engine version to use for the launched language server
    * @return either a failure or sockets that a language server listens on
    */
  def start(
    progressTracker: ActorRef,
    clientId: UUID,
    project: Project,
    version: SemVer
  ): F[ServerStartupFailure, LanguageServerSockets]

  /** Stops a language server.
    *
    * @param clientId a requester id
    * @param projectId a project id to stop
    * @return either failure or Unit representing void success
    */
  def stop(
    clientId: UUID,
    projectId: UUID
  ): F[ServerShutdownFailure, Unit]

  /** Checks if server is running or shutting down for project.
    *
    * @param projectId a project id
    * @return a tuple of booleans where the first element is true if project is open, and the second element is true if the project is currently shutting down
    */
  def isRunning(projectId: UUID): F[CheckTimeout.type, (Boolean, Boolean)]

  /** Request a language server to rename project.
    *
    * @param projectId the project id
    * @param oldName the old project name
    * @param newName the new project name
    * @return either failure or unit signaling success
    */
  def renameProject(
    projectId: UUID,
    namespace: String,
    oldName: String,
    newName: String
  ): F[ProjectRenameFailure, Unit]

  /** Kills all running servers.
    *
    * @return true if servers are killed, false otherwise
    */
  def killAllServers(): F[Throwable, Boolean]

  /** Registers a shutdown hook.
    *
    * @param projectId the project for which the hook will be registered
    * @param hook the shutdown hook to register
    * @return
    */
  def registerShutdownHook(
    projectId: UUID,
    hook: ShutdownHook[F]
  ): F[Nothing, Unit]

  /** Waits until all shutdown hooks will be fired.
    *
    * @return
    */
  def waitTillAllHooksFired(): F[Throwable, Unit]

}
