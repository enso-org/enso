package org.enso.projectmanager.infrastructure.languageserver

import java.util.UUID

import akka.actor.ActorRef
import nl.gn0s1s.bump.SemVer
import org.enso.projectmanager.data.LanguageServerSockets
import org.enso.projectmanager.model.Project

/** A language subsystem protocol.
  */
object LanguageServerProtocol {

  /** Command to start a server.
    *
    * @param clientId the requester id
    * @param project the project to start
    * @param engineVersion version of the engine to use
    * @param progressTracker an actor that should be sent notifications about
    *                        locks
    */
  case class StartServer(
    clientId: UUID,
    project: Project,
    engineVersion: SemVer,
    progressTracker: ActorRef
  )

  /** Base trait for server startup results.
    */
  sealed trait ServerStartupResult

  /** Signals that server started successfully.
    *
    * @param sockets the server sockets
    */
  case class ServerStarted(sockets: LanguageServerSockets)
      extends ServerStartupResult

  /** Base trait for server startup failures.
    */
  sealed trait ServerStartupFailure extends ServerStartupResult

  /** Signals that server boot failed with exception.
    *
    * @param throwable an exception thrown by bootloader
    */
  case class ServerBootFailed(throwable: Throwable) extends ServerStartupFailure

  /** Signals server boot timeout.
    */
  case object ServerBootTimedOut extends ServerStartupFailure

  /** Signals that previous instance of the server hasn't been shut down yet.
    */
  case object PreviousInstanceNotShutDown extends ServerStartupFailure

  /** Command to stop a server.
    *
    * @param clientId the requester id
    * @param projectId the project id
    */
  case class StopServer(clientId: UUID, projectId: UUID)

  /** A command that kills all running servers.
    */
  case object KillThemAll

  /** Signals that all servers have been killed.
    */
  case object AllServersKilled

  /** Base trait for server shutdown results.
    */
  sealed trait ServerShutdownResult

  /** Signals that server stopped successfully.
    */
  case object ServerStopped extends ServerShutdownResult

  /** Base trait for server shutdown failures.
    */
  sealed trait ServerShutdownFailure extends ServerShutdownResult

  /** Signals that server shutdown timed out.
    */
  case object ServerShutdownTimedOut extends ServerShutdownFailure

  /** Signals that an exception was thrown during stopping a server.
    *
    * @param th an exception
    */
  case class FailureDuringShutdown(th: Throwable) extends ServerShutdownFailure

  /** Signals that server wasn't started.
    */
  case object ServerNotRunning extends ServerShutdownFailure

  /** Signals that server cannot be stopped, because other clients are connected
    * to the server.
    */
  case object CannotDisconnectOtherClients extends ServerShutdownFailure

  /** Request to check is server is running.
    *
    * @param projectId the project id
    */
  case class CheckIfServerIsRunning(projectId: UUID)

  /** Signals that check timed out.
    */
  case object CheckTimeout

  /** A command requesting for project renaming.
    *
    * @param projectId the project id
    * @param oldName the old project name
    * @param newName the new project name
    */
  case class RenameProject(projectId: UUID, oldName: String, newName: String)

  /** Base trait for project rename results.
    */
  sealed trait ProjectRenameResult

  /** Signals that a project has been renamed successfully.
    */
  case object ProjectRenamed extends ProjectRenameResult

  /** Base trait for project rename failures.
    */
  sealed trait ProjectRenameFailure extends ProjectRenameResult

  /** Signals that a project is not opened.
    */
  case object ProjectNotOpened extends ProjectRenameFailure

  /** Signals that renaming operation timed out.
    */
  case object RenameTimeout extends ProjectRenameFailure

  /** Signals that cannot connect to a language server.
    */
  case object CannotConnectToServer extends ProjectRenameFailure

  /** Signals a failure during project renaming.
    *
    * @param code a failure code
    * @param message a failure message
    */
  case class RenameFailure(code: Int, message: String)
      extends ProjectRenameFailure

  /** Signals that a language server is unresponsive.
    */
  case object ServerUnresponsive extends ProjectRenameFailure

}
