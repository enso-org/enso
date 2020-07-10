package org.enso.languageserver.runtime

import org.enso.jsonrpc.Error
import org.enso.languageserver.filemanager.FileSystemFailureMapper
import org.enso.languageserver.runtime.SearchProtocol.{
  FileSystemError,
  HandlerUninitializedError,
  ModuleNotFoundError,
  SearchFailure
}

object SearchFailureMapper {

  /**
    * Maps [[SearchFailure]] into JSON RPC error.
    *
    * @param searchError the search specific failure
    * @return JSON RPC error
    */
  def mapFailure(searchError: SearchFailure): Error =
    searchError match {
      case FileSystemError(e)        => FileSystemFailureMapper.mapFailure(e)
      case HandlerUninitializedError => SearchApi.HandlerUninitializedError
      case ModuleNotFoundError(_)    => SearchApi.ModuleNotFoundError
    }

}
