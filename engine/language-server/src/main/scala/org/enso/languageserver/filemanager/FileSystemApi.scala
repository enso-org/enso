package org.enso.languageserver.filemanager

import java.io.File

/**
  * File manipulation API.
  *
  * @tparam F represents target monad
  */
trait FileSystemApi[F[_]] {

  /**
    * Writes textual content to a file.
    *
    * @param file path to the file
    * @param content a textual content of the file
    * @return either FileSystemFailure or Unit
    */
  def write(
    file: File,
    content: String
  ): F[Either[FileSystemFailure, Unit]]

}
