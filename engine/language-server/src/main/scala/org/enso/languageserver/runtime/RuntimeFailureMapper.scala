package org.enso.languageserver.runtime

import org.enso.jsonrpc._
import org.enso.languageserver.filemanager.FileSystemFailureMapper
import org.enso.languageserver.protocol.ErrorApi._
import org.enso.languageserver.runtime.ExecutionApi._
import org.enso.polyglot.runtime.Runtime.Api

object RuntimeFailureMapper {

  /**
    * Maps registry error into JSON RPC error.
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
    }

  /**
    * Maps runtime Api error into a registry error.
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
    }

}
