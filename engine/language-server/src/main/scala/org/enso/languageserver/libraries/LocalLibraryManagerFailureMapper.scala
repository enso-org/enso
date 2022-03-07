package org.enso.languageserver.libraries

import org.enso.editions.provider.EditionNotFound
import org.enso.jsonrpc
import org.enso.languageserver.filemanager.FileManagerApi.FileSystemError

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

  /** Convert the exceptions raised in the library management api to the
    * corresponding JSONRPC errors.
    *
    * @param error the raised exception
    * @return the JSONRPC error message
    */
  def mapException(error: Throwable): jsonrpc.Error =
    error match {
      case ex: LocalLibraryManagerProtocol.LocalLibraryNotFoundError =>
        LibraryApi.LocalLibraryNotFound(ex.libraryName)
      case ex: EditionNotFound =>
        LibraryApi.EditionNotFoundError(ex.editionName)
      case _ =>
        FileSystemError(error.getMessage)
    }
}
