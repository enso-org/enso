package org.enso.projectmanager.infrastructure.languageserver

import java.util.UUID

import org.enso.projectmanager.data.LanguageServerSockets
import org.enso.projectmanager.model.Project

/**
  * A language subsystem protocol.
  */
object LanguageServerProtocol {

  /**
    * Command to start a server.
    *
    * @param clientId the requester id
    * @param project the project to start
    */
  case class StartServer(clientId: UUID, project: Project)

  /**
    * Base trait for server startup results.
    */
  sealed trait ServerStartupResult

  /**
    * Signals that server started successfully.
    *
    * @param sockets the server sockets
    */
  case class ServerStarted(sockets: LanguageServerSockets)
      extends ServerStartupResult

  /**
    * Base trait for server startup failures.
    */
  sealed trait ServerStartupFailure extends ServerStartupResult

  /**
    * Signals that server boot failed with exception.
    *
    * @param throwable an exception thrown by bootloader
    */
  case class ServerBootFailed(throwable: Throwable) extends ServerStartupFailure

  /**
    * Signals server boot timeout.
    */
  case object ServerBootTimedOut extends ServerStartupFailure

  /**
    * Signals that previous instance of the server hasn't been shut down yet.
    */
  case object PreviousInstanceNotShutDown extends ServerStartupFailure

  /**
    * Command to stop a server.
    *
    * @param clientId the requester id
    * @param projectId the project id
    */
  case class StopServer(clientId: UUID, projectId: UUID)

  /**
    * Base trait for server shutdown results.
    */
  sealed trait ServerShutdownResult

  /**
    * Signals that server stopped successfully.
    */
  case object ServerStopped extends ServerShutdownResult

  /**
    * Base trait for server shutdown failures.
    */
  sealed trait ServerShutdownFailure extends ServerShutdownResult

  /**
    * Signals that server shutdown timed out.
    */
  case object ServerShutdownTimedOut extends ServerShutdownFailure

  /**
    * Signals that an exception was thrown during stopping a server.
    *
    * @param th an exception
    */
  case class FailureDuringShutdown(th: Throwable) extends ServerShutdownFailure

  /**
    * Signals that server wasn't started.
    */
  case object ServerNotRunning extends ServerShutdownFailure

  /**
    * Signals that server cannot be stopped, because other clients are connected
    * to the server.
    */
  case object CannotDisconnectOtherClients extends ServerShutdownFailure

  /**
    * Request to check is server is running.
    *
    * @param projectId the project id
    */
  case class CheckIfServerIsRunning(projectId: UUID)

  /**
    * Signals that check timed out.
    */
  case object CheckTimeout

}
