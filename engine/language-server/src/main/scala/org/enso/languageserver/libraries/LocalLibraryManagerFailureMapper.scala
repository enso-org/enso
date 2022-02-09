package org.enso.languageserver.libraries

import org.enso.jsonrpc

object LocalLibraryManagerFailureMapper {

  def mapFailure(error: LocalLibraryManagerProtocol.Failure): jsonrpc.Error =
    error match {
      case LocalLibraryManagerProtocol.InvalidSemverVersionError(version) =>
        LibraryApi.InvalidSemverVersion(version)
    }
}
