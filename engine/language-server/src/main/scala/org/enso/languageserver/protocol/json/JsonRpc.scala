package org.enso.languageserver.protocol.json

import io.circe.generic.auto._
import org.enso.jsonrpc.Protocol
import org.enso.languageserver.capability.CapabilityApi.{
  AcquireCapability,
  ForceReleaseCapability,
  GrantCapability,
  ReleaseCapability
}
import org.enso.languageserver.filemanager.FileManagerApi._
import org.enso.languageserver.io.InputOutputApi._
import org.enso.languageserver.monitoring.MonitoringApi.Ping
import org.enso.languageserver.refactoring.RefactoringApi.RenameProject
import org.enso.languageserver.runtime.ExecutionApi._
import org.enso.languageserver.search.SearchApi._
import org.enso.languageserver.runtime.VisualisationApi._
import org.enso.languageserver.session.SessionApi.InitProtocolConnection
import org.enso.languageserver.text.TextApi._

object JsonRpc {

  /** A description of supported JSON RPC messages.
    */
  val protocol: Protocol = Protocol.empty
    .registerRequest(Ping)
    .registerRequest(InitProtocolConnection)
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
    .registerRequest(RedirectStandardOutput)
    .registerRequest(RedirectStandardError)
    .registerRequest(SuppressStandardOutput)
    .registerRequest(SuppressStandardError)
    .registerRequest(FeedStandardInput)
    .registerRequest(ExecutionContextCreate)
    .registerRequest(ExecutionContextDestroy)
    .registerRequest(ExecutionContextPush)
    .registerRequest(ExecutionContextPop)
    .registerRequest(ExecutionContextRecompute)
    .registerRequest(AttachVisualisation)
    .registerRequest(DetachVisualisation)
    .registerRequest(ModifyVisualisation)
    .registerRequest(GetSuggestionsDatabase)
    .registerRequest(GetSuggestionsDatabaseVersion)
    .registerRequest(InvalidateSuggestionsDatabase)
    .registerRequest(Completion)
    .registerRequest(RenameProject)
    .registerNotification(ForceReleaseCapability)
    .registerNotification(GrantCapability)
    .registerNotification(TextDidChange)
    .registerNotification(EventFile)
    .registerNotification(ExecutionContextExpressionValuesComputed)
    .registerNotification(ExecutionContextExecutionFailed)
    .registerNotification(ExecutionContextExecutionStatus)
    .registerNotification(StandardOutputAppended)
    .registerNotification(StandardErrorAppended)
    .registerNotification(WaitingForStandardInput)
    .registerNotification(SuggestionsDatabaseUpdates)

}
