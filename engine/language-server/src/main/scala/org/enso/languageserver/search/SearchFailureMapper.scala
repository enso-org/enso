package org.enso.languageserver.search

import org.enso.jsonrpc.Error
import org.enso.languageserver.filemanager.FileSystemFailureMapper
import org.enso.languageserver.search.SearchProtocol.{
  FileSystemError,
  ModuleNameNotResolvedError,
  ProjectNotFoundError,
  SearchFailure
}

object SearchFailureMapper {

  /** Maps [[SearchFailure]] into JSON RPC error.
    *
    * @param searchError the search specific failure
    * @return JSON RPC error
    */
  def mapFailure(searchError: SearchFailure): Error =
    searchError match {
      case FileSystemError(e)            => FileSystemFailureMapper.mapFailure(e)
      case ProjectNotFoundError          => SearchApi.ProjectNotFoundError
      case ModuleNameNotResolvedError(_) => SearchApi.ModuleNameNotResolvedError
    }

}
