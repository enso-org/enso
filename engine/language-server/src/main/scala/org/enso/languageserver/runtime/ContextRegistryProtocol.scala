package org.enso.languageserver.runtime

import java.util.UUID

import enumeratum._
import org.enso.languageserver.data.ClientId
import org.enso.languageserver.filemanager.{FileSystemFailure, Path}
import org.enso.languageserver.runtime.ExecutionApi.ContextId
import org.enso.languageserver.session.JsonSession
import org.enso.text.editing.model

object ContextRegistryProtocol {

  /** Trait indicating failure response.
    */
  sealed trait Failure

  /** A request to the context registry to create a new execution context.
    *
    * @param rpcSession reference to the client
    */
  case class CreateContextRequest(rpcSession: JsonSession)

  /** A response about creation of a new execution context.
    *
    * @param contextId the newly created context's id
    */
  case class CreateContextResponse(contextId: ContextId)

  /** A request to the context registry to delete an execution context.
    *
    * @param rpcSession reference to the client
    */
  case class DestroyContextRequest(
    rpcSession: JsonSession,
    contextId: ContextId
  )

  /** A response about deletion of an execution context.
    *
    * @param contextId the newly created context's id
    */
  case class DestroyContextResponse(contextId: ContextId)

  /** A request to the context registry to push an execution context
    * down the stack.
    *
    * @param rpcSession reference to the client
    * @param contextId execution context identifier
    * @param stackItem an object representing an item on the stack
    */
  case class PushContextRequest(
    rpcSession: JsonSession,
    contextId: ContextId,
    stackItem: StackItem
  )

  /** A response about pushing the new item to the stack.
    *
    * @param contextId execution context identifier
    */
  case class PushContextResponse(contextId: ContextId)

  /** A request to the context registry to move an execution context
    * up the stack.
    *
    * @param rpcSession reference to the client
    * @param contextId execution context identifier
    */
  case class PopContextRequest(rpcSession: JsonSession, contextId: ContextId)

  /** A response about popping the stack.
    *
    * @param contextId execution context identifier
    */
  case class PopContextResponse(contextId: ContextId)

  /** A request to the context registry to recompute an execution context.
    *
    * @param rpcSession reference to the client
    * @param contextId execution context identifier
    * @param invalidatedExpressions the expressions that should be invalidated
    */
  case class RecomputeContextRequest(
    rpcSession: JsonSession,
    contextId: ContextId,
    invalidatedExpressions: Option[InvalidatedExpressions]
  )

  /** A response about recomputing the context.
    *
    * @param contextId execution context identifier
    */
  case class RecomputeContextResponse(contextId: ContextId)

  /** A notification that new information about some expressions is available.
    *
    * @param contextId execution context identifier
    * @param updates a list of updated expressions
    */
  case class ExpressionValuesComputedNotification(
    contextId: ContextId,
    updates: Vector[ExpressionValueUpdate]
  )

  /** Signals that user doesn't have access to the requested context.
    */
  case object AccessDenied extends Failure

  /** Signals that context was not found.
    *
    * @param contextId execution context identifier
    */
  case class ContextNotFound(contextId: ContextId) extends Failure

  /** Signals about file system error.
    *
    * @param error file system failure
    */
  case class FileSystemError(error: FileSystemFailure) extends Failure

  /** Signals that stack is empty.
    *
    * @param contextId execution context identifier
    */
  case class EmptyStackError(contextId: ContextId) extends Failure

  /** Signals that stack item is invalid in this context.
    *
    * @param contextId execution context identifier
    */
  case class InvalidStackItemError(contextId: ContextId) extends Failure

  /** The type of a diagnostic message. */
  sealed trait ExecutionDiagnosticKind extends EnumEntry
  object ExecutionDiagnosticKind
      extends Enum[ExecutionDiagnosticKind]
      with CirceEnum[ExecutionDiagnosticKind] {

    case object Error   extends ExecutionDiagnosticKind
    case object Warning extends ExecutionDiagnosticKind

    override val values = findValues
  }

  /** The element in the stack trace.
    *
    * @param functionName the function containing the stack call
    * @param path the location of a file
    * @param location the location of the element in a file
    */
  case class ExecutionStackTraceElement(
    functionName: String,
    path: Option[Path],
    location: Option[model.Range]
  )

  /** A diagnostic message produced as a compilation outcome.
    *
    * @param kind the type of diagnostic message
    * @param message the error message
    * @param path the file path
    * @param location the range in the source text containing a diagnostic
    * @param stack the stack trace
    */
  case class ExecutionDiagnostic(
    kind: ExecutionDiagnosticKind,
    message: String,
    path: Option[Path],
    location: Option[model.Range],
    stack: Vector[ExecutionStackTraceElement]
  )

  /** A critical failure when attempting to execute a context.
    *
    * @param message the error message
    * @param path the location of a file producing the error
    */
  case class ExecutionFailure(message: String, path: Option[Path])

  /** Signals about a critical failure in a context execution.
    *
    * @param contextId execution context identifier
    * @param failure the error description
    */
  case class ExecutionFailedNotification(
    contextId: ContextId,
    failure: ExecutionFailure
  )

  /** Signals the status of a context execution.
    *
    * @param contextId execution context identifier
    * @param diagnostics the list of diagnostic messages
    */
  case class ExecutionDiagnosticNotification(
    contextId: ContextId,
    diagnostics: Seq[ExecutionDiagnostic]
  )

  /** Requests the language server to attach a visualisation to the expression
    * specified by `expressionId`.
    *
    * @param clientId the requester id
    * @param visualisationId an identifier of a visualisation
    * @param expressionId an identifier of an expression which is visualised
    * @param visualisationConfig a configuration object for properties of the
    *                            visualisation
    */
  case class AttachVisualisation(
    clientId: ClientId,
    visualisationId: UUID,
    expressionId: UUID,
    visualisationConfig: VisualisationConfiguration
  )

  /** Signals that attaching a visualisation has succeeded.
    */
  case object VisualisationAttached

  /** Requests the language server to detach a visualisation from the expression
    * specified by `expressionId`.
    *
    * @param clientId the requester id
    * @param contextId an execution context identifier
    * @param visualisationId an identifier of a visualisation
    * @param expressionId an identifier of an expression which is visualised
    */
  case class DetachVisualisation(
    clientId: ClientId,
    contextId: UUID,
    visualisationId: UUID,
    expressionId: UUID
  )

  /** Signals that detaching a visualisation has succeeded.
    */
  case object VisualisationDetached

  /** Requests the language server to modify a visualisation.
    *
    * @param clientId  the requester id
    * @param visualisationId     an identifier of a visualisation
    * @param visualisationConfig a configuration object for properties of the
    *                            visualisation
    */
  case class ModifyVisualisation(
    clientId: ClientId,
    visualisationId: UUID,
    visualisationConfig: VisualisationConfiguration
  )

  /** Signals that a visualisation modification has succeeded.
    */
  case object VisualisationModified

  /** Represents a visualisation context.
    *
    * @param visualisationId a visualisation identifier
    * @param contextId a context identifier
    * @param expressionId an expression identifier
    */
  case class VisualisationContext(
    visualisationId: UUID,
    contextId: UUID,
    expressionId: UUID
  )

  /** An event signaling a visualisation update.
    *
    * @param visualisationContext a visualisation context
    * @param data a visualisation data
    */
  case class VisualisationUpdate(
    visualisationContext: VisualisationContext,
    data: Array[Byte]
  )

  /** Signals that a module cannot be found.
    *
    * @param moduleName the module name
    */
  case class ModuleNotFound(moduleName: String) extends Failure

  /** Signals that visualisation cannot be found.
    */
  case object VisualisationNotFound extends Failure

  /** Signals that an expression specified in a [[AttachVisualisation]] or
    * a [[ModifyVisualisation]] cannot be evaluated.
    *
    * @param message the reason of the failure
    */
  case class VisualisationExpressionFailed(message: String) extends Failure

  /** Signals that an evaluation of a code responsible for generating
    * visualisation data failed.
    *
    * @param contextId a context identifier
    * @param message the reason of the failure
    */
  case class VisualisationEvaluationFailed(contextId: UUID, message: String)

}
