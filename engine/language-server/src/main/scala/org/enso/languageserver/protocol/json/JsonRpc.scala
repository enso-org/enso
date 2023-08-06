package org.enso.languageserver.protocol.json

import io.circe.generic.auto._
import org.enso.cli.task.notifications.TaskNotificationApi.{
  TaskFinished,
  TaskProgressUpdate,
  TaskStarted
}
import org.enso.jsonrpc.Protocol
import org.enso.languageserver.ai.AICompletion
import org.enso.languageserver.capability.CapabilityApi.{
  AcquireCapability,
  ForceReleaseCapability,
  GrantCapability,
  ReleaseCapability
}
import org.enso.languageserver.filemanager.FileManagerApi._
import org.enso.languageserver.io.InputOutputApi._
import org.enso.languageserver.monitoring.MonitoringApi.{InitialPing, Ping}
import org.enso.languageserver.refactoring.RefactoringApi._
import org.enso.languageserver.runtime.ExecutionApi._
import org.enso.languageserver.search.SearchApi._
import org.enso.languageserver.runtime.VisualizationApi._
import org.enso.languageserver.session.SessionApi.InitProtocolConnection
import org.enso.languageserver.text.TextApi._
import org.enso.languageserver.libraries.LibraryApi._
import org.enso.languageserver.vcsmanager.VcsManagerApi._
import org.enso.languageserver.workspace.WorkspaceApi.ProjectInfo

object JsonRpc {

  /** A description of JSON RPC messages support during the initialization stage */
  val initProtocol: Protocol = Protocol.empty
    .registerRequest(InitialPing)
    .registerRequest(InitProtocolConnection)

  /** A description of supported JSON RPC messages at a post-initialization stage */
  def fullProtocol(init: Protocol): Protocol = init
    .registerRequest(Ping)
    .registerRequest(AcquireCapability)
    .registerRequest(ReleaseCapability)
    .registerRequest(WriteFile)
    .registerRequest(ReadFile)
    .registerRequest(CreateFile)
    .registerRequest(OpenFile)
    .registerRequest(OpenBuffer)
    .registerRequest(CloseFile)
    .registerRequest(SaveFile)
    .registerRequest(ApplyEdit)
    .registerRequest(ApplyExpressionValue)
    .registerRequest(DeleteFile)
    .registerRequest(CopyFile)
    .registerRequest(MoveFile)
    .registerRequest(ExistsFile)
    .registerRequest(ListFile)
    .registerRequest(TreeFile)
    .registerRequest(InfoFile)
    .registerRequest(ChecksumFile)
    .registerRequest(InitVcs)
    .registerRequest(SaveVcs)
    .registerRequest(StatusVcs)
    .registerRequest(ListVcs)
    .registerRequest(RestoreVcs)
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
    .registerRequest(ExecutionContextSetExecutionEnvironment)
    .registerRequest(ExecutionContextInterrupt)
    .registerRequest(ExecutionContextGetComponentGroups)
    .registerRequest(ExecuteExpression)
    .registerRequest(AttachVisualization)
    .registerRequest(DetachVisualization)
    .registerRequest(ModifyVisualization)
    .registerRequest(GetSuggestionsDatabase)
    .registerRequest(GetSuggestionsDatabaseVersion)
    .registerRequest(InvalidateSuggestionsDatabase)
    .registerRequest(Completion)
    .registerRequest(AICompletion)
    .registerRequest(RenameProject)
    .registerRequest(RenameSymbol)
    .registerRequest(ProjectInfo)
    .registerRequest(EditionsListAvailable)
    .registerRequest(EditionsResolve)
    .registerRequest(EditionsGetProjectSettings)
    .registerRequest(EditionsSetParentEdition)
    .registerRequest(EditionsSetLocalLibrariesPreference)
    .registerRequest(EditionsListDefinedLibraries)
    .registerRequest(EditionsListDefinedComponents)
    .registerRequest(LibraryListLocal)
    .registerRequest(LibraryCreate)
    .registerRequest(LibraryGetMetadata)
    .registerRequest(LibrarySetMetadata)
    .registerRequest(LibraryGetPackage)
    .registerRequest(LibraryPublish)
    .registerRequest(LibraryPreinstall)
    .registerNotification(TaskStarted)
    .registerNotification(TaskProgressUpdate)
    .registerNotification(TaskFinished)
    .registerNotification(ForceReleaseCapability)
    .registerNotification(GrantCapability)
    .registerNotification(TextDidChange)
    .registerNotification(FileAutoSaved)
    .registerNotification(FileModifiedOnDisk)
    .registerNotification(EventFile)
    .registerNotification(ContentRootAdded)
    .registerNotification(ContentRootRemoved)
    .registerNotification(ExecutionContextExpressionUpdates)
    .registerNotification(ExecutionContextExecutionFailed)
    .registerNotification(ExecutionContextExecutionComplete)
    .registerNotification(ExecutionContextExecutionStatus)
    .registerNotification(StandardOutputAppended)
    .registerNotification(StandardErrorAppended)
    .registerNotification(WaitingForStandardInput)
    .registerNotification(SuggestionsDatabaseUpdates)
    .registerNotification(VisualizationEvaluationFailed)
    .finalized()
}
