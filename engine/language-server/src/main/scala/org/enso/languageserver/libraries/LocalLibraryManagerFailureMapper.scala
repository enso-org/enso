package org.enso.languageserver.libraries

import org.enso.jsonrpc

/** The object mapping the [[LocalLibraryManagerProtocol]] failures into the
  * corresponding JSONRPC error messages.
  */
object LocalLibraryManagerFailureMapper {

  /** Convert the [[LocalLibraryManagerProtocol.Failure]] into the corresponding
    * JSONRPC error message.
    *
    * @param error the failure object
    * @return the JSONRPC error message
    */
  def mapFailure(error: LocalLibraryManagerProtocol.Failure): jsonrpc.Error =
    error match {
      case LocalLibraryManagerProtocol.InvalidSemverVersionError(version) =>
        LibraryApi.InvalidSemverVersion(version)
    }
}
