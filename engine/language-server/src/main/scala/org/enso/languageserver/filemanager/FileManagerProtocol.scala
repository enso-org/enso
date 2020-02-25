package org.enso.languageserver.filemanager

object FileManagerProtocol {

  /**
    * Requests the Language Server write textual content to an arbitrary file.
    *
    * @param path a path to a file
    * @param content a textual content
    */
  case class FileWrite(path: Path, content: String)

  /**
    * Signals file manipulation status.
    *
    * @param result either file system failure or unit representing success
    */
  case class FileWriteResult(result: Either[FileSystemFailure, Unit])

}
