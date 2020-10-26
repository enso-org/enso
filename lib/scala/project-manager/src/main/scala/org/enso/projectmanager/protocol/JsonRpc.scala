package org.enso.projectmanager.protocol

import io.circe.generic.auto._
import org.enso.jsonrpc.Protocol
import org.enso.projectmanager.protocol.ProjectManagementApi._
import org.enso.projectmanager.protocol.TaskProgressAPI._

object JsonRpc {

  /** A description of supported JSON RPC messages.
    */
  lazy val protocol: Protocol =
    Protocol.empty
      .registerRequest(ProjectCreate)
      .registerRequest(ProjectDelete)
      .registerRequest(ProjectOpen)
      .registerRequest(ProjectClose)
      .registerRequest(ProjectRename)
      .registerRequest(ProjectList)
      .registerNotification(TaskStarted)
      .registerNotification(TaskProgressUpdate)
      .registerNotification(TaskFinished)

}
