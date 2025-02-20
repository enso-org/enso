package org.enso.projectmanager.protocol

import io.circe.generic.auto._
import org.enso.jsonrpc.Protocol
import org.enso.projectmanager.protocol.ProjectManagementApi._
import org.enso.projectmanager.protocol.FileSystemManagementApi._
import org.enso.cli.task.notifications.TaskNotificationApi._

/** Implicits from this module are required for correct serialization.
  *
  * Do not remove this import.
  */
import org.enso.semver.SemVerJson._
import org.enso.projectmanager.infrastructure.file.FileJson._

object JsonRpc {

  /** A description of supported JSON RPC messages.
    */
  lazy val protocol: Protocol =
    Protocol.empty
      .registerRequest(ProjectCreate)
      .registerRequest(ProjectStatus)
      .registerRequest(ProjectDelete)
      .registerRequest(ProjectOpen)
      .registerRequest(ProjectClose)
      .registerRequest(ProjectRename)
      .registerRequest(ProjectList)
      .registerRequest(ProjectDuplicate)
      .registerNotification(TaskStarted)
      .registerNotification(TaskProgressUpdate)
      .registerNotification(TaskFinished)
      .registerRequest(EngineListAvailable)
      .registerRequest(EngineListInstalled)
      .registerRequest(EngineInstall)
      .registerRequest(EngineUninstall)
      .registerRequest(ConfigGet)
      .registerRequest(ConfigSet)
      .registerRequest(ConfigDelete)
      .registerRequest(LoggingServiceGetEndpoint)
      .registerRequest(FileSystemList)
      .registerRequest(FileSystemExists)
      .registerRequest(FileSystemCreateDirectory)
      .registerRequest(FileSystemDeleteDirectory)
      .registerRequest(FileSystemMoveDirectory)
      .registerRequest(FileSystemReadPath)
      .registerRequest(FileSystemWritePath)
      .finalized()

}
