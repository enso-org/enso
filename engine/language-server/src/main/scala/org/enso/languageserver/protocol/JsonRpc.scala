package org.enso.languageserver.protocol

import io.circe.generic.auto._
import org.enso.jsonrpc.Protocol
import org.enso.languageserver.capability.CapabilityApi.{
  AcquireCapability,
  ForceReleaseCapability,
  GrantCapability,
  ReleaseCapability
}
import org.enso.languageserver.filemanager.FileManagerApi._
import org.enso.languageserver.text.TextApi._
import org.enso.languageserver.runtime.ExecutionApi._

object JsonRpc {

  /**
    * A description of supported JSON RPC messages.
    */
  val protocol: Protocol = Protocol.empty
    .registerRequest(AcquireCapability)
    .registerRequest(ReleaseCapability)
    .registerRequest(WriteFile)
    .registerRequest(ReadFile)
    .registerRequest(CreateFile)
    .registerRequest(OpenFile)
    .registerRequest(CloseFile)
    .registerRequest(SaveFile)
    .registerRequest(ApplyEdit)
    .registerRequest(DeleteFile)
    .registerRequest(CopyFile)
    .registerRequest(MoveFile)
    .registerRequest(ExistsFile)
    .registerRequest(ListFile)
    .registerRequest(TreeFile)
    .registerRequest(InfoFile)
    .registerRequest(ExecutionContextCreate)
    .registerRequest(ExecutionContextDestroy)
    .registerRequest(ExecutionContextPush)
    .registerRequest(ExecutionContextPop)
    .registerNotification(ForceReleaseCapability)
    .registerNotification(GrantCapability)
    .registerNotification(TextDidChange)
    .registerNotification(EventFile)

}
