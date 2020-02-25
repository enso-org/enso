package org.enso.languageserver.filemanager

import org.enso.languageserver.filemanager.FileManagerApi.{
  AccessDeniedError,
  ContentRootNotFoundError,
  FileSystemError
}
import org.enso.languageserver.jsonrpc.Error

object FileSystemFailureMapper {

  def mapFailure(fileSystemFailure: FileSystemFailure): Error =
    fileSystemFailure match {
      case ContentRootNotFound              => ContentRootNotFoundError
      case AccessDenied                     => AccessDeniedError
      case GenericFileSystemFailure(reason) => FileSystemError(reason)
    }

}
