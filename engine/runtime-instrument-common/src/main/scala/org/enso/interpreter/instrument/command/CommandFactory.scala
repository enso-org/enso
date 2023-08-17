package org.enso.interpreter.instrument.command

import org.enso.polyglot.runtime.Runtime.Api

/** A factory that creates a command for an API request.
  */
object CommandFactory {

  /** Creates a command that encapsulates a function request as an object.
    *
    * @param request an API request
    * @return a command
    */
  def createCommand(request: Api.Request): Command =
    request.payload match {
      case payload: Api.CreateContextRequest =>
        new CreateContextCmd(request.requestId, payload)

      case payload: Api.PushContextRequest =>
        new PushContextCmd(request.requestId, payload)

      case payload: Api.PopContextRequest =>
        new PopContextCmd(request.requestId, payload)

      case payload: Api.DestroyContextRequest =>
        new DestroyContextCmd(request.requestId, payload)

      case payload: Api.RecomputeContextRequest =>
        new RecomputeContextCmd(request.requestId, payload)

      case payload: Api.InterruptContextRequest =>
        new InterruptContextCmd(request.requestId, payload)

      case _: Api.GetComponentGroupsRequest =>
        new GetComponentGroupsCmd(request.requestId)

      case payload: Api.AttachVisualization =>
        new AttachVisualizationCmd(request.requestId, payload)

      case payload: Api.DetachVisualization =>
        new DetachVisualizationCmd(request.requestId, payload)

      case payload: Api.ModifyVisualization =>
        new ModifyVisualizationCmd(request.requestId, payload)

      case payload: Api.RenameProject =>
        new RenameProjectCmd(request.requestId, payload)

      case payload: Api.RenameSymbol =>
        new RenameSymbolCmd(request.requestId, payload)

      case payload: Api.OpenFileNotification =>
        new OpenFileCmd(payload)
      case payload: Api.CloseFileNotification => new CloseFileCmd(payload)
      case payload: Api.EditFileNotification  => new EditFileCmd(payload)
      case payload: Api.SetExpressionValueNotification =>
        new SetExpressionValueCmd(payload)

      case payload: Api.InvalidateModulesIndexRequest =>
        new InvalidateModulesIndexCmd(request.requestId, payload)

      case _: Api.GetTypeGraphRequest =>
        new GetTypeGraphCommand(request.requestId)

      case payload: Api.DeserializeLibrarySuggestions =>
        new DeserializeLibrarySuggestionsCmd(request.requestId, payload)

      case _: Api.StartBackgroundProcessing =>
        new StartBackgroundProcessingCmd(request.requestId)

      case payload: Api.SerializeModule =>
        new SerializeModuleCommand(request.requestId, payload.module)

      case payload: Api.SetExecutionEnvironmentRequest =>
        new SetExecutionEnvironmentCommand(
          request.requestId,
          payload.contextId,
          payload.executionEnvironment
        )

      case Api.ShutDownRuntimeServer() =>
        throw new IllegalArgumentException(
          "ShutDownRuntimeServer request is not convertible to command object"
        )

      case _: Api.AcquireLockRequest | _: Api.ReleaseLockRequest =>
        throw new IllegalArgumentException(
          "Lock-related requests are not meant to be handled by the runtime."
        )
    }

}
