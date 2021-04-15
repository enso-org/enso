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
          result.map(toProtocolError)
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
  private def toProtocolError(
    error: Api.ExecutionResult.Failure
  ): ContextRegistryProtocol.ExecutionFailure =
    ContextRegistryProtocol.ExecutionFailure(
      error.message,
      error.file.flatMap(config.findRelativePath)
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
