package org.enso.languageserver.runtime

import org.enso.jsonrpc.Error
import org.enso.languageserver.data.Config
import org.enso.languageserver.filemanager.FileSystemFailureMapper
import org.enso.languageserver.protocol.json.ErrorApi._
import org.enso.languageserver.runtime.ExecutionApi._
import org.enso.polyglot.runtime.Runtime.Api

final class RuntimeFailureMapper(config: Config) {

  /** Maps runtime Api error into a registry error.
    *
    * @param error runtime Api error
    * @return registry error
    */
  def mapApiError(error: Api.Error): ContextRegistryProtocol.Failure =
    error match {
      case Api.ContextNotExistError(contextId) =>
        ContextRegistryProtocol.ContextNotFound(contextId)
      case Api.EmptyStackError(contextId) =>
        ContextRegistryProtocol.EmptyStackError(contextId)
      case Api.InvalidStackItemError(contextId) =>
        ContextRegistryProtocol.InvalidStackItemError(contextId)
      case Api.ModuleNotFound(moduleName) =>
        ContextRegistryProtocol.ModuleNotFound(moduleName)
      case Api.VisualisationExpressionFailed(message, result) =>
        ContextRegistryProtocol.VisualisationExpressionFailed(
          message,
          result.map(toProtocolDiagnostic)
        )
      case Api.VisualisationNotFound() =>
        ContextRegistryProtocol.VisualisationNotFound
    }

  /** Convert the runtime failure message to the context registry protocol
    * representation.
    *
    * @param error the error message
    * @return the registry protocol representation fo the diagnostic message
    */
  def toProtocolFailure(
    error: Api.ExecutionResult.Failure
  ): ContextRegistryProtocol.ExecutionFailure =
    ContextRegistryProtocol.ExecutionFailure(
      error.message,
      error.file.flatMap(config.findRelativePath)
    )

  /** Convert the runtime diagnostic message to the context registry protocol
    * representation.
    *
    * @param diagnostic the diagnostic message
    * @return the registry protocol representation of the diagnostic message
    */
  def toProtocolDiagnostic(
    diagnostic: Api.ExecutionResult.Diagnostic
  ): ContextRegistryProtocol.ExecutionDiagnostic =
    ContextRegistryProtocol.ExecutionDiagnostic(
      toDiagnosticType(diagnostic.kind),
      diagnostic.message,
      diagnostic.file.flatMap(config.findRelativePath),
      diagnostic.location,
      diagnostic.expressionId,
      diagnostic.stack.map(toStackTraceElement)
    )

  /** Convert the runtime diagnostic type to the context registry protocol
    * representation.
    *
    * @param kind the diagnostic type
    * @return the registry protocol representation of the diagnostic type
    */
  private def toDiagnosticType(
    kind: Api.DiagnosticType
  ): ContextRegistryProtocol.ExecutionDiagnosticKind =
    kind match {
      case Api.DiagnosticType.Error() =>
        ContextRegistryProtocol.ExecutionDiagnosticKind.Error
      case Api.DiagnosticType.Warning() =>
        ContextRegistryProtocol.ExecutionDiagnosticKind.Warning
    }

  /** Convert the runtime stack trace element to the context registry protocol
    * representation.
    *
    * @param element the runtime stack trace element
    * @return the registry protocol representation of the stack trace element
    */
  private def toStackTraceElement(
    element: Api.StackTraceElement
  ): ContextRegistryProtocol.ExecutionStackTraceElement =
    ContextRegistryProtocol.ExecutionStackTraceElement(
      element.functionName,
      element.file.flatMap(config.findRelativePath),
      element.location,
      element.expressionId
    )

}
object RuntimeFailureMapper {

  /** Create runtime failure mapper instance.
    *
    * @param config the language server config
    * @return a new instance of [[RuntimeFailureMapper]]
    */
  def apply(config: Config): RuntimeFailureMapper =
    new RuntimeFailureMapper(config)

  /** Maps registry error into JSON RPC error.
    *
    * @param error registry error
    * @return JSON RPC error
    */
  def mapFailure(error: ContextRegistryProtocol.Failure): Error =
    error match {
      case ContextRegistryProtocol.AccessDenied =>
        AccessDeniedError
      case ContextRegistryProtocol.ContextNotFound(_) =>
        ContextNotFoundError
      case ContextRegistryProtocol.FileSystemError(error) =>
        FileSystemFailureMapper.mapFailure(error)
      case ContextRegistryProtocol.EmptyStackError(_) =>
        EmptyStackError
      case ContextRegistryProtocol.InvalidStackItemError(_) =>
        InvalidStackItemError
      case ContextRegistryProtocol.VisualisationNotFound =>
        VisualisationNotFoundError
      case ContextRegistryProtocol.ModuleNotFound(name) =>
        ModuleNotFoundError(name)
      case ContextRegistryProtocol.VisualisationExpressionFailed(msg, result) =>
        VisualisationExpressionError(msg, result)
    }
}
